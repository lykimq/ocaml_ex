
(* This function to check_errors for the generated memtrace files *)

let check_errors () =
  (* val putenv: string -> string -> unit 
      putenv name value: sets the value associated to a variable in the process environment
    `name` is the name of the enviroment variable
    `value` its new associated value 
  *)
  Unix.putenv "MEMTRACE" "/bad/file/name";
  (* Check it with trace *)
  (* val trace_if_requested : ?context:string -> ?sampling_rate:float -> unit -> unit 
    If the Memtrace environment is set, begin tracing to the file it specifies, 
    and continue tracing until the process exits.
    
    The context is an abitrary string, which is logged in the trace. 
    It is useful to identify trace files.

    The sampling_rate is the proportion of allocated words that should be sampeld.

    Values larger than about 1e-4 will have some performance impact. 
    The sampling rate can also be specified with the Memtrace_rate environment variable.
    If both are used, the env var takes percedence. 
  *)
  (match Memtrace.trace_if_requested () with 
  | _ -> assert false
  | exception (Invalid_argument _) -> ()
  );
  (* second check for good file sotre in tmp folder *)
  Unix.putenv "MEMTRACE" "/tmp/goodfilename" ;
  (match Memtrace.trace_if_requested ~sampling_rate:(-3.) () with 
  | _ -> assert false
  | exception (Invalid_argument _) -> ());
  (* 3rd check for the good trace where the memtrace_rate is 42 *)
  Unix.putenv "MEMTRACE" "/tmp/goodfilename";
  Unix.putenv "MEMTRACE_RATE" "42";
  (match Memtrace.trace_if_requested () with 
  | _ -> assert false
  | exception (Invalid_argument _ ) -> ());
  (* 4th check with the memtrace name is potato*)
  Unix.putenv "MEMTRACE" "/tmp/goodfilename" ;
  Unix.putenv "MEMTRACE_RATE" "potato";
  (match Memtrace.trace_if_requested () with 
  | _ -> assert false
  | exception (Invalid_argument _) -> ())


let () = check_errors ()

let globs = Array.make 1000 [||]

let nglobs = ref 0

(* store ref of an array size 1000 starting with 0 then keep on increase it *)
let leak x = globs.(!nglobs) <- x; incr nglobs 

let rec long_bt = function 
| 0 ->
    leak (Array.make 1000 0);
    (Sys.opaque_identity List.iter) (fun () ->
      leak (Array.make 1000 0)) [()];
      42
| n ->
  if Random.bool () then 
    1 + long_bt (n - 1)
else 
  2 + long_bt (n - 1)

let go () =
  (* 
    val temp_file: ?temp_dir:string -> string -> string -> string 
    tempfile prefix suffix: returns the name of a fresh temporary file in the temporary
    directory.
    The base name of the temporary file is formed by:
    concatenating prefix, then a suitably chosen integer number, then suffix.

    The optional argument `temp_dir` indicates the temporary directory to use, 
    defaulting to the current result of Filename.get_temp_dir_name.

    The temporary file is created empty, 
    with permissions 0o600 (readable and writable only by the file owner). 

    The file is guaranteed to be different from any other file that existed when temp_file was called.
  *)
  let filename = Filename.temp_file "memtrace-trace" ".ctf" in 
  (* Manually start tracing 

     val start_tracing: 
     context:string option ->
     sampling_rate:float ->
     filename:string -> tracer
  *)
  let trace = Memtrace.start_tracing ~context:(Some "ctx") ~sampling_rate:0.1 ~filename in 
  leak (Array.make 4242 42);
  (* call function long_bt and loop them 10 times *)
  for _i = 0 to 10 do 
    let n = long_bt 10_000 in 
    assert (n > 0);
  done;
  (* loop from 1 to 1000 for trace free *)
  for _i = 1 to 1000 do 
    (* 
      Use external to track non-GC-heap allocations in Memtrace trace

      [alloc ~bytes] reports an allocation of a given number of bytes

      If tracing is enabled, a small fraction of the calls to this function will return 
      [Some tok], where [tok] should be passed to [free] when the object if freed

      val alloc : bytes:int -> token option
      val free: token -> unit
    *)
    Option.iter Memtrace.External.free 
    (Memtrace.External.alloc ~bytes:((Sys.word_size / 8) * 7))
  done;
  (* manually stop tracing *)
  Memtrace.stop_tracing trace;

  (* Start to read the tracing result from the file trace above *)
  let read_trace = Memtrace.Trace.Reader.open_ ~filename in 
  let first = ref true in 
  let n_long = ref 0 in 
  let last_ext = ref None in 
  let ext_samples = ref 0 in 
  (* Iterate the file trace read and then testing the content of the tracing
     results
  *)
  Memtrace.Trace.Reader.iter read_trace (fun _ ev -> 
    match ev with 
    (* it is in the Event type t in memtrace/src/trace.ml 
      type t =
      | Alloc of {
        obj_id : Obj_id.t;
        length: int;
        ...
      }   
    | Promote of Obj_id.t;
    | Collect of Obj_id.t
    *)
    | Alloc info when !first ->
        first := false;
        (* check the first trace is 4242 *)
        assert (info.length = 4242);
        ()
    | Alloc info when info.length = 1000 ->
      (* backtraces should be truncated *)
      assert (info.backtrace_length > 3500 && info.backtrace_length < 4000);
      incr n_long
    | Alloc info when info.length = 7 ->
      last_ext := Some info.obj_id;
      ext_samples := !ext_samples + info.nsamples;
    | Collect id -> 
      assert (!last_ext = Some id);
      last_ext := None 
    | e -> failwith ("unexpected " ^ 
      (Memtrace.Trace.Event.to_string 
        (Memtrace.Trace.Reader.lookup_location_code read_trace) e)));
  (* after reading the file, time to close it *)
  Memtrace.Trace.Reader.close read_trace;
  (* Now using the Unix unlink to file name
      val unlink: string -> unit
      removes the named file
  *)
   Unix.unlink  filename;
  (*check the content of the file again *)
  assert (650 <= !ext_samples && !ext_samples < 750);
  assert (not !first);
  assert (!n_long = 20)


let () =
  go (); go () 
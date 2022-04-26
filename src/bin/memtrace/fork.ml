(*
https://blog.janestreet.com/finding-memory-leaks-with-memtrace/
https://www.ocamlpro.com/blog/2020_12_01_memthol_exploring_program_profiling  

https://www.dra27.uk/blog/platform/2020/10/08/windows-memtrace.html
https://github.com/dra27/leaky

https://github.com/janestreet/memtrace/tree/master/test

test.ml in the test folder above

build: dune build 

dune exec -- ./fork.exe

The memtrace files will be generated in the "/home/quyen/ocaml_ex/src/bin/memtrace" folder

- View the file:

memtrace-viewer memtrace_fork08b868.ctf

open: http://quyen:8080/

formating with ocamlformat:
dune build @fmt --auto-promote

*)

let test_fork ~quick_exit () =
  let filename =
    Filename.temp_file ~temp_dir:"/home/quyen/ocaml_ex/src/bin/memtrace"
      "memtrace_fork" ".ctf" in
  Unix.putenv "MEMTRACE" filename;
  (* Start memtrace tracing *)
  let trace = Memtrace.start_tracing ~context:None ~sampling_rate:1. ~filename in
  (* set up the allocation: before, after, child *)
  let alloc_before = 1234
  and alloc_after = 7364
  and alloc_child = 42 in
  (* starting from alloc before -> child -> after *)
  (*
     Sys.opaque_identity : 'a -> 'a
     For the purposes of optimatization, opaque_identity behaves like an unknown
     (thus possibly side-effecting) function
     A typical use of this function is to prevent pure computations from begin
     optimized away in benchmarking loops. for example:
     for _round = 1 to 100_000 do
         ignore (Sys.opaque_identity (my_pure_computation ()))
     done

     The example below the pure function we want to get is:
     Array.make alloc_before "a": create an array size:alloca_before fill with "a"
  *)
  let _ = Sys.opaque_identity Array.make alloc_before "a" in
  (* alloc child *)
  (* trace the unix fork command
      val fork: unit -> int
      For a new process. The returned interger is 0 for the child process,
      the pid of the child process for the parent process.
  *)
  begin
    match Unix.fork () with
    | 0 ->
      let count = if quick_exit then 1 else 1_000_000 in
      for _i = 1 to count do
        ignore (Sys.opaque_identity Array.make alloc_child "a")
      done;
      exit 0
    | pid ->
    (* Unix.waitpid: wait_flag list -> int -> int * process_status
       Same as wait (wait until one oe fhte children processes die, and returns its pid
       and termination status), but waits for the child process whose pid is given.
       if a pid is:
         -`-1`: means wait for the chlid.
         -`0`: means wait for any child in the same process group as the current process.
         - negative pid argument: represent process groups.
       The list of options indicates whethere `waitpid` should return immediately without
       waiting, and whether it should report stopped children.
    *)
    match Unix.waitpid [] pid with
    | _, WEXITED 0 -> ()
    | _ -> assert false
  end;
  (* alloc after *)
  let _ = Sys.opaque_identity Array.make alloc_after "a" in
  (* stop memtrace trace *)
  Memtrace.stop_tracing trace;

  (* Memtrace.Trace modules:
     - Writer: writing traces
     - Reader: reading traces
  *)

  (* create module R from Memetrace.Trace.Reader *)
  let module R = Memtrace.Trace.Reader in
  (* val open_: filename:string -> t *)
  let trace = R.open_ ~filename in
  (* create a hashtable of size 20 *)
  let sizes = Hashtbl.create 20 in
  (* iterate over a trace
     iter: t -> ?parse_backtraces:bool -> (Timedelta.t -> Event.t -> unit) -> unit
  *)
  R.iter trace (fun _time ev ->
      match ev with
      | Alloc a ->
        Hashtbl.add sizes a.length
          () (* add the allocation length a into the hash table*)
      | _ -> ());

  (* check the size of alloc_before/alloca_after is it a member of the hash table *)
  assert (Hashtbl.mem sizes alloc_before);
  assert (Hashtbl.mem sizes alloc_after);
  assert (not (Hashtbl.mem sizes alloc_child));
  ()

let () = test_fork ~quick_exit:false ()
let () = test_fork ~quick_exit:true ()

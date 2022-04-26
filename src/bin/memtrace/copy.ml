(*
   dune build
   dune exec -- ./copy.exe ocamlopt.ctf ocamlopt.ctf.copy

   the memtrace filenamectf store in the /user/tmp folder
*)
open Memtrace.Trace

let copy inf outf =
  (* Reader: reading traces
     val open_: filename:string -> t
     open trace file:inf to read
  *)
  let trace = Reader.open_ ~filename:inf in

  (* Unix.openfile: string -> open_flag list -> file_perm -> file_descr

     open the named file with the given flags.
     - The 3rd argument is the permission to give to the file if it is created
     Return a file descripter on the named file.

     type open_flag:
     - O_CREAT: create if nonexistent
     - O_WRONGLY: open for writing
     - O_TRUNC: truncate to 0 length if existing

     file_perm = int
     The type of file access rights, e.g. `0o64` is read and write for user,
     read for group, none for others.
  *)
  let wfd = Unix.openfile outf [O_CREAT; O_WRONLY; O_TRUNC] 0o600 in

  (* val info: t -> Info.t

     module Info is a global trace info
     type t =
     - sample_rate
     - word_size
     - executable_name
     - host_name
     - ocaml_runtime_params
     - pid
     - start_time
     - context

     Reading the info of the file r (reading trace from file:inf)
  *)
  let info = Reader.info trace in
  (* Write: writing trace
     val create: Unix.file_descr -> ?getpid:(unit -> int64) -> Info.t -> t

     Writing trace for file wfd with the info is pid and info
  *)
  let w = Writer.create wfd ~getpid:(fun () -> info.pid) info in
  (* iterate over a trace *)
  Reader.iter trace (fun now ev ->
      (*
         val put_event: t -> decode_callstack_entry:(Location_code.t -> Location.t list)
         -> Timestamp.t
         -> Event.t
         -> unit
      *)
      Writer.put_event w
        ~decode_callstack_entry:(fun loc ->
          (* val lookup_location_code: t -> Location_code.t -> Location.t list
             - Location_code :  codes for subsequences of locations in a backtrace
          *)
          Reader.lookup_location_code trace loc)
        (* Timedelta: times measured from the start of the trace
           val offset: Timestamp.t -> t -> Timestamp.t
        *)
        (Timedelta.offset info.start_time now)
        ev);
  (* close: t -> unit *)
  Reader.close trace;
  (* val flush: t -> unit  flush file buffers to disk *)
  Writer.flush w;
  (* val close: file_descr -> unit: close a file descriptor *)
  Unix.close wfd

let () =
  (*
    Sys: is a module system interface

    val argv: string array
    The command line arguments given to the process. 
    - The first element is the command name used to invoke the program.
    - The following elements are the command-line arguments given to the program.
  *)
  match Sys.argv with
  | [|_; inf; outf|] -> copy inf outf
  | _ ->
    Printf.fprintf stderr "usage: copy <in> <out>\n%!";
    exit 1

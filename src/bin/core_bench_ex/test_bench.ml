(**
   build and run execution:
   $ dune build
   or if dune declare as executable (to build as native (.exe) or bytecode (.bc) versions):
   $ dune exec hello_world
   
   - for test_metrics.ml
   $ dune build
   $ dune exec -- ./test_metrics.exe
*)

(*
Example link: https://dune.readthedocs.io/en/latest/quick-start.html

https://github.com/janestreet/core_bench/tree/master/test

*)

open Core

let main () =
  Command_unix.run (
    Command.group 
    ~summary:"Several benchmarks"
    ["basic", Basic_tests.command])

let () = main ()

(***************************************************************)
(* Another way to write bench using core_bench *)
(*
let polymorphic_compare () =
  let cmp a b = Stdlib.(if a > b then a else b) in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let monomorphic_compare () =
  let cmp (a:int) (b:int) = Stdlib.(if a > b then a else b) in
  for i = 0 to 1000 do
    ignore(cmp 0 i)
  done

let tests =
  [ "Polymorphic comparison", polymorphic_compare;
    "Monomorphic comparison", monomorphic_compare ]

let () =
  List.map tests ~f:(fun (name,test) -> Bench.Test.create ~name test)
  |> Bench.make_command
  |> Command.run*)
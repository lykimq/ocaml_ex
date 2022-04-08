
(*
https://github.com/Chris00/ocaml-benchmark/blob/master/examples/iter.ml
Doc: https://chris00.github.io/ocaml-benchmark/doc/benchmark/Benchmark/index.html

Result
quyen@🏵 :~/ocaml_ex/src/bin/ocaml_benchmark$ dune build
quyen@🏵 :~/ocaml_ex/src/bin/ocaml_benchmark$ dune exec ./iter.exe
Entering directory '/home/quyen/ocaml_ex/src'
Throughputs for "ba" running 3 times for at least 3 CPU seconds:
ba:  3.19 WALL ( 3.18 usr +  0.00 sys =  3.18 CPU) @ 271640.26/s (n=863050)
     3.27 WALL ( 3.25 usr +  0.00 sys =  3.25 CPU) @ 265379.84/s (n=863050)
     3.32 WALL ( 3.32 usr +  0.00 sys =  3.32 CPU) @ 260299.48/s (n=863050)
Iterating a function with a simple side effect: 
       Rate       ba
ba 265773+-4406/s --


Run the tests "ba" and three times for at least 3 seconds each, 
printing the results of each test, and then print a cross tabulation of the results:

wall : float;	 (** Wallclock time (in seconds) *)

*)

open Bigarray 

let n = 1_000

type vec = (float, float64_elt, c_layout) Array1.t

let a = Array1.create float64 c_layout n 
let () = Array1.fill a 1.

let ba f (x: vec) = 
  for i = 0 to n - 1 do 
    f (Array1.unsafe_get x i)
  done 



open Benchmark 

let () = 
  (* Simulate a simple side effect *)
  let z = ref 0. in 
  let f x = z := x in 

  let res = throughputN  ~repeat:3 3
  [
    ("ba", (fun () -> ba f a), ())
  ]
in 
print_endline "Iterating a function with a simple side effect: ";
tabulate res

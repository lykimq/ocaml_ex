(*
  https://github.com/Chris00/ocaml-benchmark/blob/master/examples/numbers.ml
*)

open Printf
open Benchmark

(* test the speed of addition for native ints (unboxed), and Int32/Int64  (which are both boxed). *)

let f_int n =
  let rec loop i sum =
    if i < n 
    then  loop (i + 1) (sum + 1)
    else sum 
  in 
  loop 0 0

let f_int32 n =
  let rec loop i sum =
    if i < n 
    then loop (i + 1) (Int32.add sum Int32.one)
    else sum
  in 
  Int32.to_int (loop 0 Int32.zero)

let f_int64 n =
  let rec loop i sum =
    if i < n 
    then loop (i + 1) (Int64.add sum Int64.one)
    else sum  
  in 
  Int64.to_int (loop 0 Int64.zero)   


(* Main function for benchmarks *)
let () =
  (* print out the results of the f_* functions to doublecheck that 
  they work as we intend  *)
  printf "f_int 777 = %d\n" (f_int 777);
  printf "f_int32 777 = %d\n" (f_int32 777);
  printf "f_int64 777 = %d\n" (f_int64 777);
  print_newline ();

  (* let's exercise the *1 functions:
     1000L: mean Latencies for 1000 iterations 
     5: mean each function running for at least 5 CPU seconds
     note that, it is only testing the functon f_int.
  *)
  let _ = latency1 ~name:"int-1-latency1" 1000L f_int 10_000 in 
  let _ = throughput1 ~name:"int-1-throughput1" 5 f_int 10_000 in 
  print_newline ();

  (* let's exercise the *N functions:  *)
  (* the part for benchmark with throughputN
    - 5: each running 5 times 
    - 10: each running at least 10 CPU seconds
  *)
  let res = 
    throughputN ~repeat:5 10
    [
      ("int", f_int, 10_000);
      ("int32", f_int32, 10_000);
      ("int64", f_int64, 10_000)
    ]
  in 
  print_newline ();
  tabulate res;

  (* the part for benchmark latencyN *)
  print_newline ();
  let res = 
    latencyN 2000L [
      ("int", f_int, 10_000);
      ("int32", f_int32, 10_000);
      ("int64", f_int64, 10_000)
    ]
  in 
  print_newline ();
  tabulate res


  (* Result
  quyen@🏵 :~/ocaml_ex/src/bin/ocaml_benchmark$ dune build
  quyen@🏵 :~/ocaml_ex/src/bin/ocaml_benchmark$ dune exec ./numbers.exe
Entering directory '/home/quyen/ocaml_ex/src'
f_int 777 = 777                    
f_int32 777 = 777
f_int64 777 = 777

Latencies for 1000 iterations of "int-1-latency1":
int-1-latency1:  0.01 WALL ( 0.01 usr +  0.00 sys =  0.01 CPU) @ 158679.78/s (n=1000)
                (warning: too few iterations for a reliable count)
Throughputs for "int-1-throughput1" running for at least 5 CPU seconds:
int-1-throughput1:  5.16 WALL ( 5.15 usr +  0.00 sys =  5.15 CPU) @ 191250.44/s (n=984366)

Throughputs for "int", "int32", "int64" each running 5 times for at least 10 CPU seconds:
  int: 11.22 WALL (11.20 usr +  0.00 sys = 11.20 CPU) @ 177474.02/s (n=1988012)
       11.36 WALL (11.34 usr +  0.00 sys = 11.34 CPU) @ 175337.45/s (n=1988012)
       11.52 WALL (11.51 usr +  0.00 sys = 11.51 CPU) @ 172711.82/s (n=1988012)
       11.41 WALL (11.39 usr +  0.00 sys = 11.39 CPU) @ 174553.90/s (n=1988012)
       11.44 WALL (11.40 usr +  0.02 sys = 11.41 CPU) @ 174201.37/s (n=1988012)
int32: 10.45 WALL (10.43 usr +  0.00 sys = 10.43 CPU) @ 46345.07/s (n=483262)
       10.48 WALL (10.44 usr +  0.01 sys = 10.45 CPU) @ 46240.68/s (n=483262)
       10.64 WALL (10.60 usr +  0.01 sys = 10.61 CPU) @ 45561.24/s (n=483262)
       10.90 WALL (10.88 usr +  0.00 sys = 10.88 CPU) @ 44411.14/s (n=483262)
       10.51 WALL (10.48 usr +  0.00 sys = 10.48 CPU) @ 46120.99/s (n=483262)
int64: 10.94 WALL (10.91 usr +  0.00 sys = 10.91 CPU) @ 68360.47/s (n=745890)
       11.15 WALL (11.13 usr +  0.00 sys = 11.13 CPU) @ 67017.68/s (n=745890)
       10.85 WALL (10.81 usr +  0.01 sys = 10.82 CPU) @ 68950.68/s (n=745890)
       11.54 WALL (11.50 usr +  0.01 sys = 11.51 CPU) @ 64804.80/s (n=745890)
       10.81 WALL (10.79 usr +  0.00 sys = 10.79 CPU) @ 69099.60/s (n=745890)

          Rate       int32 int64   int
int32  45736+- 680/s    --  -32%  -74%
int64  67647+-1520/s   48%    --  -61%
  int 174856+-1484/s  282%  158%    --

Latencies for 2000 iterations of "int", "int32", "int64":
  int:  0.02 WALL ( 0.02 usr +  0.00 sys =  0.02 CPU) @ 123632.32/s (n=2000)
       (warning: too few iterations for a reliable count)
int32:  0.05 WALL ( 0.05 usr +  0.00 sys =  0.05 CPU) @ 39057.16/s (n=2000)
       (warning: too few iterations for a reliable count)
int64:  0.04 WALL ( 0.04 usr +  0.00 sys =  0.04 CPU) @ 51859.15/s (n=2000)
       (warning: too few iterations for a reliable count)

          Rate int32 int64   int
int32  39057/s    --  -25%  -68%
int64  51859/s   33%    --  -58%
  int 123632/s  217%  138%    --
  
  *)
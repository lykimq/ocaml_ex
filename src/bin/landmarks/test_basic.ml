(* compile
dune build --instrument-with landmarks ./test_basic.exe

* Run with ocaml_landmark on:
OCAML_LANDMARKS=on _build/default/test_basic.exe

Call graph '_build/default/test_basic.exe':
-------------------------------------------
[   10.56G cycles in 5 calls ]     - 100.00% : Test_basic.zzz

Note: Nodes accounting for less than 1.00% of their parent have been ignored.

Aggregated table:
----------------
          Name;        Filename;    Calls;     Time
          ROOT; src/landmark.ml;        0;   10.56G
Test_basic.zzz; test_basic.ml:1;        5;   10.56G


* run with profiling (define in dune-workspace)
dune exec --context profiling ./test_basic.exe

Call graph '_build/profiling/test_basic.exe':
---------------------------------------------
[   10.56G cycles in 5 calls ]     - 100.00% : Test_basic.zzz

Note: Nodes accounting for less than 1.00% of their parent have been ignored.

Aggregated table:
----------------
          Name;         Filename;    Calls;     Time
          ROOT;  src/landmark.ml;        0;   10.56G
Test_basic.zzz; test_basic.ml:29;        5;   10.56G

* profiling-auto 
dune exec --context profiling-auto ./test_basic.exe
Call graph '_build/profiling-auto/test_basic.exe':
--------------------------------------------------
[   10.56G cycles in 1 calls ]     - 100.00% : load(test_basic)
[   10.56G cycles in 1 calls ]     |   - 100.00% : Test_basic.main
[    8.45G cycles in 2 calls ]     |   |   - 80.00% : Test_basic.f
[    8.45G cycles in 4 calls ]     |   |   |   - 100.00% : Test_basic.zzz
[    2.11G cycles in 1 calls ]     |   |   - 20.00% : Test_basic.zzz

Note: Nodes accounting for less than 1.00% of their parent have been ignored.

Aggregated table:
----------------
            Name;        Filename;    Calls;     Time
            ROOT; src/landmark.ml;        0;   10.56G
load(test_basic); test_basic.ml:1;        1;   10.56G
 Test_basic.main; test_basic.ml:7;        1;   10.56G
  Test_basic.zzz; test_basic.ml:1;        5;   10.56G
    Test_basic.f; test_basic.ml:3;        2;    8.45G

*)

let[@landmark] zzz () = Unix.sleep 1

let f () =
  zzz ();
  zzz ()

let main () =
  zzz ();
  f ();
  f ()

let () = main ()
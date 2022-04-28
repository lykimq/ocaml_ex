(*
   https://github.com/c-cube/qcheck

   Qcheck (QuickCheck) inspired property-based testing for OCaml, and 
   combinators to generate random values to run tests on
*)


let passing =
  QCheck.Test.make ~count:100 ~long_factor:100
  ~name:"list_rev_is_involutive"
  QCheck.(list small_int)
  (fun l -> List.rev (List.rev l) = l)



let stats_tests =
  let open QCheck in
  [
    Test.make ~name:"state_display_test_1" ~count:1000
      (add_stat ("dist", fun x -> x) small_signed_int)
      (fun _ -> true);
  ]

(* using @ to add more test functions *)
let main =
  QCheck_runner.run_tests_main [passing]
  @ stats_tests

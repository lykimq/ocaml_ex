
(*
Alcotest: is a system for registering and runnign tests
Qcheck: is an alternative implementation of quickcheck    
*)

let () = Alcotest.run "Yaml" [ ("Yaml", Test_yaml.tests)]
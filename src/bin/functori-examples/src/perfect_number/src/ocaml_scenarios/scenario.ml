(* This file is only generated once, but as long as it exists it 
will not be overwritten, you can safely write in it. To find 
examples for your scenarios, you can look at scenarios.example.ml *)
open Pn_ocaml_interface
open Tzfunc.Rp

let main () =
  let _ = Tzfunc.Node.set_silent true in
  Format.printf "Deploying the contract@.";
  let>? perfect_number_kt1, _op_hash =
    deploy
    ~node:Blockchain.ithaca_node
    ~name:"perfect_number"
    ~from:Blockchain.alice_flextesa
    ~amount:10000L
    Z.one
  in
  Format.printf "KT1 : %s@." perfect_number_kt1;
  let>? operation_hash =
    call__default
      ~node:Blockchain.ithaca_node
      ~from:Blockchain.bob_flextesa
      ~kt1:perfect_number_kt1
      (Z.of_int 496)
  in
  Format.printf "[Propose 496] Operation hash: %s@." operation_hash;
 (*let>? operation_hash =
    call__default
      ~node:Blockchain.ithaca_node
      ~from:Blockchain.bob_flextesa
      ~kt1:perfect_number_kt1
      (Z.of_int 12)
  in
  Format.printf "[Propose 12] Operation hash: %s@." operation_hash;*)
  Lwt.return_ok ()

let _ =
  Lwt_main.run (main ())

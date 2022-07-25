(* For now this file is a copy of "new_scenario_toolkit" from
   spice_tezos. This is for development purposes; eventually ginger
   should be directly and automatically used with the generated
   code. *)

open Tzfunc
open Proto
open Crypto

(* let script_of_contract ~filename =
 *   let bytes =
 *     let ic = Unix.open_process_in cmd in
 *     let s = read ic in
 *     close_in ic;
 *     s in
 *   Tzfunc.Proto.(Bytes (Crypto__.H.mk bytes)) *)

type identity = {sk : string; pk : A.pk; pkh : A.pkh}

let str_of_identity id =
  Format.sprintf "{\nsk : %s;\npk : %s;\npkh : %s\n}" id.sk id.pk id.pkh

(* Traditional bootstrap1, bootstrap2 and bootstrap3 of the Tezos sandbox *)

let bootstrap1 : identity =
  {
    pkh = "tz1id1nnbxUwK1dgJwUra5NEYixK37k6hRri";
    pk = "edpkv2Gafe23nz4x4hJ1SaUM3H5hBAp5LDwsvGVjPsbMyZHNomxxQA";
    sk = "edsk4LhLg3212HzL7eCXfCWWvyWFDfwUS7doL4JUSmkgTe4qwZbVYm";
  }

let bootstrap2 : identity =
  {
    pkh = "tz1Tny451rJY5vqUs7JHoYpJHdMH5znNxuxw";
    pk = "edpkuW2GZePnF1rt3XrM533PYaRiXPwaHzCpHQSbsNzAds1SVXbhkm";
    sk = "edsk4DDEDf3VrBYYuhVgyjfP3VC1RVX7ZJHAsCMKV511U1a7nNxJdH";
  }

let bootstrap3 : identity =
  {
    pkh = "tz1Zebr6mNxTPTsbfkvNjAJTmaxGMFbqmhc9";
    pk = "edpku8ehu8waYZKfjvLWwJ6Pv7UuPvdKgszknNnUVj7MTyf4R9tyZA";
    sk = "edsk2z8cbf8LGkp1LsD8k7EBDYEMjFvWFv7EY3edr3cyGxzu4fyfqk";
  }

(* Flextesa agents: Alice and Bob *)

let bob_flextesa : identity =
  {
    pkh = "tz1aSkwEot3L2kmUvcoxzjMomb9mvBNuzFK6";
    pk = "edpkurPsQ8eUApnLUJ9ZPDvu98E8VNj4KtJa1aZr16Cr5ow5VHKnz4";
    sk = "edsk3RFfvaFaxbHx8BMtEW1rKQcPtDML3LXjNqMNLCzC3wLC1bWbAt";
  }

let alice_flextesa : identity =
  {
    pkh = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb";
    pk = "edpkvGfYw3LyB1UcCahKQk4rF2tvbMUk8GFiTuMjL75uGXrpvKXhjn";
    sk = "edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq";
  }

let get_identity (seed : string) : identity =
  let sk = Tzfunc.Crypto.Sk.T.mk @@ Raw.mk seed in
  let pk = Crypto.Sk.to_public_key sk in
  let curve = `ed25519 in
  let pkh = Pkh.b58enc ~curve @@ Pk.hash pk in
  let sk = Sk.b58enc ~curve sk in
  let pk = Pk.b58enc ~curve pk in
  {pkh; sk; pk}

let generate_identity () =
  let curve = `ed25519 in
  let pk, sk = Ed25519.keypair () in
  let pkh = Pkh.b58enc ~curve @@ Pk.hash pk in
  {sk = Sk.b58enc ~curve sk; pk = Pk.b58enc ~curve pk; pkh}

(* code and storage can be either:
   Bytes (H.mk "00a5ee...")
   Micheline (Mprim { prim="Pair"; annots=[]; args=[...] }).
   Mprim ... can be retrieved from a json file using
   EzEncoding.destruct micheline_enc.Encoding.json json_string *)

let sandbox_node = "http://127.0.0.1:18731"

let flextesa_node = "http://localhost:20000"

let ithaca_node = "https://ithacanet.ecadinfra.com"

let default_node = sandbox_node

let default_base = EzAPI.BASE default_node

let main_base = EzAPI.BASE "https://tz.functori.com"

let get_balance ?(base = default_base) ~addr () =
  let open Tzfunc.Rp in
  let>? acc = Tzfunc__Node.get_account_info ~base addr in
  let bal = acc.ac_balance in
  Lwt.return_ok bal

let get_storage ?(base = default_base) ?(debug = false) kt1 decode =
  let open Tzfunc.Rp in
  let>? storage = Tzfunc.Node.get_storage ~base kt1 in
  if debug then
    Format.eprintf
      "Storage: %s\n%!"
      (EzEncoding.construct
         Tzfunc.Proto.script_expr_enc.json
         (Micheline storage)) ;
  Lwt.return_ok @@ decode storage
(*   match storage with
 * | Micheline m -> Lwt.return_ok @@ decode m
 * | _ -> failwith "storage is not micheline" *)

let make_transfer ?(amount = 0L) ?(fee = -1L) ?(gas_limit = Z.minus_one)
    ?(storage_limit = Z.minus_one) ?(counter = Z.zero) ?(node = sandbox_node)
    ~from ~dst () =
  let open Tzfunc.Rp in
  let op =
    {
      man_info =
        {
          source = from.pkh;
          kind = Transaction {amount; destination = dst; parameters = None};
        };
      man_numbers = {fee; gas_limit; storage_limit; counter};
      man_metadata = None;
    }
  in

  let sign b = Node.sign ~edsk:from.sk b in
  let base = EzAPI.BASE node in
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in
  let get_pk () = Lwt.return_ok from.pk in
  let>? bytes, protocol, branch, ops =
    Node.forge_manager_operations ~forge_method:`local ~base ~get_pk [op]
  in
  let>? hash = Node.inject ~base ~sign ~bytes ~branch ~protocol ops in
  let>? () = Utils.wait_next_block ~base () in
  let>? () = Utils.wait_next_block ~base () in
  Lwt.return_ok hash

let make_mass_transfer_ops ?(amount = 0L) ?(fee = -1L)
    ?(gas_limit = Z.minus_one) ?(storage_limit = Z.minus_one)
    ?(counter = Z.zero) ~from (dsts : string list) =
  List.mapi
    (fun i dst ->
      {
        man_info =
          {
            source = from.pkh;
            kind = Transaction {amount; destination = dst; parameters = None};
          };
        man_numbers =
          {fee; gas_limit; storage_limit; counter = Z.(counter + of_int i)};
        man_metadata = None;
      })
    (List.map (fun x -> x) dsts)

let make_mass_transfer ?(node = sandbox_node) ~amount ~from dsts =
  let open Tzfunc.Rp in
  let ops = make_mass_transfer_ops ~from ~amount dsts in
  let base = EzAPI.BASE node in
  let sign b = Node.sign ~edsk:from.sk b in
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in
  let get_pk () = Lwt.return_ok from.pk in
  let>? bytes, protocol, branch, ops =
    Node.forge_manager_operations ~forge_method:`local ~base ~get_pk ops
  in
  let>? hash = Node.inject ~base ~sign ~bytes ~branch ~protocol ops in
  let>? () = Utils.wait_next_block ~base () in
  let>? () = Utils.wait_next_block ~base () in
  Lwt.return_ok hash

let inject_existing_transferops ?(debug = false) ?(node = sandbox_node) ~from ops =
  let open Tzfunc.Rp in
  if debug then Format.eprintf "entering inject_existing_transferops\n%!" ;
  let base = EzAPI.BASE node in
  let sign b = Node.sign ~edsk:from.sk b in
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in
  let get_pk () = Lwt.return_ok from.pk in
  Lwt.bind
    (Node.forge_manager_operations ~forge_method:`local ~base ~get_pk ops)
  @@ function
  | Error e ->
      Format.eprintf
        "[Error in inject_existing_transferops]: %s\nThe operation was %s%!\n"
        (Tzfunc.Rp.string_of_error e)
        (EzEncoding.construct
           (Json_encoding.list (manager_operation_enc script_expr_enc).json)
           ops) ;
      Lwt.return_error e
  | Ok (bytes, protocol, branch, ops) -> (
      if debug then Format.eprintf "Successful forge of transaction\n%!" ;
      Lwt.bind (Node.inject ~base ~sign ~bytes ~branch ~protocol ops)
      @@ function
      | Error e ->
          Format.eprintf
            "[Error at injection of transaction]: %s%!\n"
            (Tzfunc.Rp.string_of_error e) ;
          Lwt.return_error e
      | Ok hash ->
          let>? () = Utils.wait_next_block ~base () in
          let>? () = Utils.wait_next_block ~base () in
          Lwt.return_ok @@ hash)

(*   let>? (bytes, protocol, branch, ops) =
 *   Node.forge_manager_operations ~local_forge:false ~base ~get_pk ops
 * in
 * let>? hash = Node.inject ~base ~sign ~bytes ~branch ~protocol ops in
 * Utils.wait_operation hash @@ function _ ->
 * Lwt.return_ok hash *)

let call_entrypoint ?(debug = true) ?(amount = 0L) ?(fee = -1L)
    ?(gas_limit = Z.minus_one) ?(storage_limit = Z.minus_one)
    ?(counter = Z.zero) ?(node = sandbox_node) ~from ~dst param =
  let open Tzfunc.Rp in
  let get_pk () = Lwt.return_ok from.pk in
  let op =
    {
      man_info =
        {
          source = from.pkh;
          kind =
            Transaction {amount; destination = dst; parameters = Some param};
        };
      man_numbers = {fee; gas_limit; storage_limit; counter};
      man_metadata = None;
    }
  in
  let base = EzAPI.BASE node in
  let sign b = Node.sign ~edsk:from.sk b in
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in

  if debug then
    Format.eprintf
      "Sending param: %s\n%!"
      (EzEncoding.construct script_expr_enc.json param.value) ;
  (match param.entrypoint with
  | EPnamed _entrypoint ->
      ()
      (* Format.eprintf "boxi client call %s from %s --entrypoint %s --arg '$(cat fa2_initial_storage.mic)'
         %!" dst from.pkh entrypoint *)
  | _ ->
      ()
      (* Format.eprintf "other type of entrypoint
         %!" *)) ;
  Lwt.bind
    (Node.forge_manager_operations ~forge_method:`both ~base ~get_pk [op])
  @@ function
  | Error e ->
      Format.eprintf
        "[Error in forge_manager_operations (call_entrypoint)]: %s\n\
         The operation was %s%!\n"
        (Tzfunc.Rp.string_of_error e)
        (EzEncoding.construct (manager_operation_enc script_expr_enc).json op) ;
      Lwt.return_error e
  | Ok (bytes, protocol, branch, ops) -> (
      if debug then Format.eprintf "Successful forge of transaction\n%!" ;
      Lwt.bind (Node.inject ~base ~sign ~bytes ~branch ~protocol ops)
      @@ function
      | Error e ->
          Format.eprintf
            "[Error at injection of transaction]: %s%!\n"
            (Tzfunc.Rp.string_of_error e) ;
          Lwt.return_error e
      | Ok hash ->
          let>? () = Utils.wait_next_block ~base () in
          let>? () = Utils.wait_next_block ~base () in
          Lwt.return_ok @@ hash)

let create_deploy_op ?(amount = 0L) ?(fee = -1L) ?(gas_limit = Z.minus_one)
    ?(storage_limit = Z.minus_one) ?(counter = Z.zero) ~from ~code storage =
  {
    man_info =
      {
        source = from.pkh;
        kind = Origination {balance = amount; script = {code; storage}};
      };
    man_numbers = {fee; gas_limit; storage_limit; counter};
    man_metadata = None;
  }

let deploy_existing_op ?(debug = false) ?(node = sandbox_node) ~from op =
  let open Tzfunc.Rp in
  let get_pk () = Lwt.return_ok from.pk in
  let base = EzAPI.BASE node in
  (* let get_pk () = Lwt.return_ok from.pk in *)
  let sign b = Node.sign ~edsk:from.sk b in
  (* Format.eprintf "Sending code: %s
     and storage %s
     %!" (EzEncoding.construct script_expr_enc.json code) (EzEncoding.construct script_expr_enc.json storage); *)
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in
  Lwt.bind
    (Node.forge_manager_operations ~forge_method:`local ~base ~get_pk [op])
  @@ function
  | Error e ->
      Format.eprintf
        "[Error in forge_manager_operations (deploy)]: %s\n\
         The operation was %s\n\
         %!"
        (Tzfunc.Rp.string_of_error e)
        (EzEncoding.construct (manager_operation_enc script_expr_enc).json op) ;
      Lwt.return_error e
  | Ok (bytes, protocol, branch, ops) -> (
      if debug then Format.eprintf "Successful forge\n%!" ;
      Lwt.bind (Node.inject ~base ~sign ~bytes ~branch ~protocol ops)
      @@ function
      | Error e ->
          Format.eprintf
            "[Error at injection]: %s%!\n"
            (Tzfunc.Rp.string_of_error e) ;
          Lwt.return_error e
      | Ok hash ->
          let>? () = Utils.wait_next_block ~base () in
          let>? () = Utils.wait_next_block ~base () in
          (* Format.eprintf "Ok hash %s
             %!" hash; *)
          Lwt.return_ok @@ op_to_KT1 hash)
(* let>? bytes,protocol,branch,ops = Node.forge_manager_operations ~local_forge:false ~base ~get_pk [op] in
 * let>? hash = Node.inject ~base ~sign ~bytes ~branch ~protocol ops in
 * Utils.wait_operation hash @@ function _ ->
 * Lwt.return_ok @@ op_to_KT1 hash *)

let deploy ?(debug = false) ?(amount = 0L) ?(fee = -1L) ?(gas_limit = Z.minus_one)
    ?(storage_limit = Z.minus_one) ?(counter = Z.zero) ?(node = sandbox_node)
    ?(name = "No provided name") ~from ~code storage =
  let open Tzfunc.Rp in
  let get_pk () = Lwt.return_ok from.pk in
  let op =
    create_deploy_op
      ~amount
      ~fee
      ~gas_limit
      ~storage_limit
      ~counter
      ~from
      ~code
      storage
  in
  let base = EzAPI.BASE node in
  (* let get_pk () = Lwt.return_ok from.pk in *)
  let sign b = Node.sign ~edsk:from.sk b in
  (* Format.eprintf "Sending code: %s
     and storage %s
     %!" (EzEncoding.construct script_expr_enc.json code) (EzEncoding.construct script_expr_enc.json storage); *)
  let _ = Node.set_constants ~base () in
  let _ = Node.set_node node in
  Lwt.bind
    (Node.forge_manager_operations ~forge_method:`local ~base ~get_pk [op])
  @@ function
  | Error e ->
      Format.eprintf
        "[Error in forge_manager_operations (deploy %s)]: %s\n\
         The initial storage was %s\n\
         %!"
        name
        (Tzfunc.Rp.string_of_error e)
        (EzEncoding.construct script_expr_enc.json storage)
      (* (EzEncoding.construct manager_operation_enc.json op) *) ;
      Lwt.return_error e
  | Ok (bytes, protocol, branch, ops) -> (
      if debug then Format.eprintf "Successful forge\n%!" ;
      Lwt.bind (Node.inject ~base ~sign ~bytes ~branch ~protocol ops)
      @@ function
      | Error e ->
          Format.eprintf
            "[Error at injection]: %s%!\n"
            (Tzfunc.Rp.string_of_error e) ;
          Lwt.return_error e
      | Ok hash ->
          if debug then Format.eprintf "Ok hash %s\n%!" hash ;
          let kt1 = op_to_KT1 hash in
          if debug then Format.eprintf "KT1: %s\n%!" kt1 ;
          let>? () = Utils.wait_next_block ~base:(EzAPI.BASE node) () in
          let>? () = Utils.wait_next_block ~base:(EzAPI.BASE node) () in
          Lwt.return_ok @@ (op_to_KT1 hash, hash))
let parallel_calls (f : 'a -> unit) (l : (unit -> ('a, 'b) result Lwt.t) list) =
  let open Tzfunc.Rp in
  let operations = Lwt_list.map_p (fun f -> f ()) l in
  let> operations_hashes = operations in
  List.iter (Result.iter f) operations_hashes;
  Lwt.return (Ok ())

(* let get_storage kt1 = Node.get_storage kt1 *)

(* let check ?(node=default_node) ~contract f =
 *   let base = EzAPI.BASE node in
 *   Lwt.bind (Node.get_account_info ~base contract) @@ function
 *   | Error e -> Lwt.return_error e
 *   | Ok a -> f a *)
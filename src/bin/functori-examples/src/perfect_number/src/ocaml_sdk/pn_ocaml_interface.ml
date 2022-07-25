open Pn_code
open Factori_types
open Tzfunc.Proto
type _default = int_michelson
let _default_encode : _default -> micheline = int_michelson_encode
let _default_decode (m : micheline) : _default = int_michelson_decode m
let _default_micheline = int_michelson_micheline
let _default_generator () = int_michelson_generator ()

let call__default ?(node = Blockchain.default_node) ?(debug=false) ?(amount=0L) ~from ~kt1 (param : _default) =
     let param =
     {
     entrypoint = EPnamed "default";
     value = Micheline (_default_encode param);
     } in
     Blockchain.call_entrypoint ~debug ~node ~amount ~from ~dst:kt1 param

type storage = int_michelson
let storage_encode : storage -> micheline = int_michelson_encode
let storage_decode (m : micheline) : storage = int_michelson_decode m
let storage_micheline = int_michelson_micheline
let storage_generator () = int_michelson_generator ()

let deploy ?(amount=0L) ?(node="https://tz.functori.com") ?(name="No name provided") ?(from=Blockchain.bootstrap1) storage =
               let storage = storage_encode storage in
               Blockchain.deploy ~amount ~node ~name ~from ~code (Micheline storage)

let test_storage_download ~kt1 ~base () =
     let open Tzfunc.Rp in
     let open Blockchain in
     Lwt_main.run @@
     let>? storage = get_storage ~base ~debug:(!Factori_types.debug > 0) kt1 storage_decode in
     let storage_reencoded = storage_encode storage in
     Lwt.return_ok @@ Factori_types.output_debug @@ Format.asprintf "Done downloading storage: %s."
     (Ezjsonm_interface.to_string
     (Json_encoding.construct
     micheline_enc.json
     storage_reencoded))
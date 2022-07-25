open Tzfunc.Proto
open Factori_types
(*Type definition for _default *)
type _default = int_michelson
val _default_encode : _default -> micheline
val _default_decode : micheline -> _default
val _default_generator : unit -> _default
val _default_micheline : micheline


val call__default :   ?node:string -> ?debug:bool -> ?amount:int64 -> from:Blockchain.identity ->
                kt1:Tzfunc.Proto.A.contract ->
                _default -> (string, Tzfunc__.Rp.error) result Lwt.t

(*Type definition for storage *)
type storage = int_michelson
val storage_encode : storage -> micheline
val storage_decode : micheline -> storage
val storage_generator : unit -> storage
val storage_micheline : micheline


val deploy :             ?amount:int64 ->
                         ?node:string ->
                         ?name:string ->
                         ?from:Blockchain.identity ->
                         storage -> (string * string, Tzfunc__.Rp.error) result Lwt.t
                         
val test_storage_download : 
kt1:Proto.A.contract -> base:EzAPI__Url.TYPES.base_url -> unit -> (unit, Tzfunc__.Rp.error) result
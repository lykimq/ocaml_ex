open Tzfunc.Proto
open Ez_file

let get_code filename : script_expr =
  if not (FileString.exists filename) then (
    Format.eprintf "Could not find file: %s\n" filename ;
    raise Not_found)
  else
    let s = FileString.read_file filename in
    Micheline (EzEncoding.destruct Tzfunc.Proto.(micheline_enc.Encoding.json) s)

let get_code_from_path path name : script_expr =
  let ( // ) x y = FileString.concat x y in
  let filename = path // (name ^ ".json") in
  get_code filename

let pack ?(print = false) michtyp michval =
  let open Format in
  if print then
    eprintf
      "Calling pack on\ntype ==> %s\nand value ==> %s\n\n%!"
      (EzEncoding.construct Tzfunc.Proto.micheline_enc.json michtyp)
      (EzEncoding.construct Tzfunc.Proto.micheline_enc.json michval) ;
  match Tzfunc.Forge.pack michtyp michval with
  | Ok packed -> Result.Ok packed
  | Error e ->
      Format.eprintf "%s" (Tzfunc.Rp.string_of_error e) ;
      Result.error e
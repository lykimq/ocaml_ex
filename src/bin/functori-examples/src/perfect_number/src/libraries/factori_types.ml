open Tzfunc.Proto

let debug = ref 0
let set_debug level = debug := level

let output_debug ?(level=1) msg = if !debug >= level then (if level=0 then Format.eprintf "%s" msg else Format.eprintf "[debug_level %d]%s%!" !debug msg) else ()
(* Utils *)

(* utils for or types *)
let is_leaf = function
  | Mprim {prim = `Left; _} -> false
  | Mprim {prim = `Right; _} -> false
  | _ -> true

(* useful for sum types *)
let mich_annot ~annot = function
  | Mprim {prim; args; annots} -> Mprim {prim; args; annots = annot :: annots}
  | m -> m

exception EncodeError of string

exception DecodeError of script_expr * string

(* let ligo_compile_expression ?(filename=None) (expr : string) *)

let print_decode_error_raw se ppf s =
  Format.fprintf
    ppf
    "[DecodeError]%s:\n%s\n%!"
    s
    (EzEncoding.construct script_expr_enc.json se)

let print_decode_error_raw_light ppf s =
  Format.fprintf ppf "[DecodeError<light printing>]%s\n%!" s

let print_decode_error se s =
  output_debug ~level:2 @@ Format.asprintf "%a" (print_decode_error_raw se) s

let print_decode_error_light _se s =
  output_debug ~level:1 @@ Format.asprintf "%a" print_decode_error_raw_light s

let print_decode_error ?(level=1) se s =
  if level = 1 then print_decode_error_light se s
  else if level >= 2 then print_decode_error se s
  else ()

let wrap_decode f x =
  try f x with DecodeError (se, s) -> print_decode_error se s

let wrap_decode2 f x y = wrap_decode (f x) y

let wrap_decode3 f x y z = wrap_decode2 (f x) y z

let fail_on_micheline msg expr =
  let s =
    EzEncoding.construct Tzfunc.Proto.(micheline_enc.Encoding.json) expr
  in
  raise
    (DecodeError
       ( Micheline expr,
         Printf.sprintf "[failed on micheline expression]%s: %s" msg s ))

(** Not terminal recursive for now, but it is called on small types  *)
let extract_list_from_pair ?(msg = "record type") x =
  output_debug ~level:1 @@ Format.sprintf "[extract_list_from_pair] %s" msg;
  let unique_tag = Random.int 1000 in
  output_debug ~level:2 @@ Format.asprintf
    "[extract_list_from_pair%d] %s\n%!"
    unique_tag
    (EzEncoding.construct Tzfunc.Proto.micheline_enc.json x) ;
  let rec aux = function
    | Tzfunc.Proto.Mprim {prim = `Pair; args; _} ->
        List.concat (List.map aux args)
    | m -> [m]
    (* fail_on_micheline (Printf.sprintf "Record <%s>" msg) m *)
  in
  let res = aux x in
  output_debug ~level:2 @@ Format.asprintf
    "[extract_list_from_pair%d] result =====> %s\n\n%!"
    unique_tag
    (EzEncoding.construct
       (Json_encoding.list Tzfunc.Proto.micheline_enc.json)
       res) ;
  res

let is_left_or_right = function
  | Mprim {prim = `Left; _} -> true
  | Mprim {prim = `Right; _} -> true
  | _ -> false

type layout = L_tree | L_comb

let default_layout = L_tree

let pow2 n =
  let rec aux res = function 0 -> res | k -> aux (2 * res) (k - 1) in
  aux 1 n



let make_unit () : micheline = Mprim {prim = `Unit; args = []; annots = []}

let unit_micheline : micheline = Mprim {prim = `unit; args = []; annots = []}

let unit_micheline_with_annot annot : micheline =
  Mprim {prim = `unit; args = []; annots = [annot]}


(* End Utils *)

type lambda_params = {from : micheline; to_ : micheline; body : micheline}

type nat = A.zarith

type int_michelson = Z.t

type address = string

type bytes = hex (* Crypto.hash.t *)

type ('a, 'b) map = ('a * 'b) list

type ('a, 'b) big_map = Literal of ('a * 'b) list | Abstract of A.zarith

type ('a, 'b) lambda = Lambda of lambda_params

type timestamp = string

type contract = Contract of micheline

type operation = string

type signature = string

type key = string

type tez = int_michelson

type key_hash = string

type ('a, 'b) michelson_pair = 'a * 'b

type 'a set = 'a list

type never_type = Never              (* I could make an empty type in OCaml, but I'm not sure it's necessary *)

type chain_id = string

type 'a ticket = Ticket of 'a

(* Encode and decode *)

let micheline_decode = function
  | Micheline m -> m
  | _ -> failwith "not a micheline expression"

let inject_prim p = Mprim {prim = p; args = []; annots = []}

let unit_encode () = make_unit ()

let unit_decode = function
  | Mprim {prim = `Unit; args = []; annots = []} -> ()
  | m -> fail_on_micheline "Invalid unit" m

let tuple1_decode adec m = adec m

let tuple2_encode aenc benc (x, y) =
  Mprim {prim = `Pair; args = [aenc x; benc y]; annots = []}

let make_pair l = Mprim {prim = `Pair; args = l; annots = []}

let flatten m =
  output_debug
  @@ Format.sprintf
       "[flatten] Entering with argument %s\n%!"
       (EzEncoding.construct micheline_enc.json m) ;
  let rec flatten_aux first = function
    | Mprim {prim = `Pair; args = l; _} ->
        List.concat (List.map (flatten_aux false) l)
    | Mseq l when first -> List.concat (List.map (flatten_aux false) l)
    | x -> [x]
  in
  let res = flatten_aux true m in
  output_debug
  @@ Format.sprintf
       "[flatten] Exiting with output %s\n%!"
       (EzEncoding.construct (Json_encoding.list micheline_enc.json) res) ;
  res


(* The type of a Pair configuration, for example [[1];[1]] for a
   traditional pair or [[2];[1][1]] *)
type configuration = Int.t list

let print_configuration ppf (c : configuration) =
  let open Format in
  fprintf
    ppf
    "(%a)"
    (pp_print_list
       ~pp_sep:(fun ppf _ -> fprintf ppf ";")
       (fun ppf x -> fprintf ppf "%d" x))
    c

exception AllPartitionsFailed of (configuration * (script_expr * string)) list

let print_all_partitions_failure ?(light_printing = true) ppf errs =
  let rec aux = function
    | [] -> ()
    | (c, (se, s)) :: errs ->
        let error_light = Format.asprintf "%a" print_decode_error_raw_light s in
        let error = Format.asprintf "%a" (print_decode_error_raw se) s in
        Format.fprintf
          ppf
          "(%a,%s)"
          print_configuration
          c
          (if light_printing then error_light else error) ;
        aux errs
  in
  aux errs


let print_list ppf l =
  let open Format in
  fprintf
    ppf
    "%s"
    (EzEncoding.construct (Json_encoding.list micheline_enc.json) l)

(* The configuration of [[a];[b;c];[d]] is [1;2;1] *)
let get_configuration (l : 'a list list) : configuration =
  List.map List.length l

let print_configuration_of_micheline_list n l =
  let open Format in
  output_debug
  @@ asprintf
       "configuration for %d: %a\n(%a)\n%!"
       n
       (pp_print_list
          ~pp_sep:(fun ppf _ -> fprintf ppf " ")
          (fun ppf x -> fprintf ppf "%d" (List.length x)))
       l
       (pp_print_list
          ~pp_sep:(fun ppf _ -> fprintf ppf " ")
          (fun ppf x ->
            fprintf
              ppf
              "%s"
              (EzEncoding.construct (Json_encoding.list micheline_enc.json) x)))
       l

exception EmptyList

(* n >=2 *)
let try_all_partitions_multi n l (f : 'a list list -> 'b) =
  let open Format in
  output_debug ~level:2 @@
    asprintf
      "[try_all_partitions_multi] Entering with n=%d and input %a!\n"
      n
      print_list
      l ;
  let rec findkposts k (l : micheline list) =
    if List.length l = k + 1 then [List.map (fun x -> [x]) l]
    else
      match l with
      | [] -> raise EmptyList (* or [] ? *)
      | [x] -> if k = 0 then [[[x]]] else []
      | x1 :: xs ->
          List.map (fun y -> [x1] :: y) (findkposts (k - 1) xs)
          @ List.map
              (function
                | (y1 :: ys : 'a list list) -> (x1 :: y1) :: ys | [] -> [[x1]])
              (findkposts k xs)
  in
  let rec aux errs = function
    | [] ->
        output_debug @@ sprintf "No winning configuration for n=%d\n%!" n ;
        raise (AllPartitionsFailed errs)
    | l :: ls -> (
        output_debug ~level:2 "Trying configuration: " ;
        print_configuration_of_micheline_list n l ;
        try
          let res = f l in
          output_debug ~level:1 @@ sprintf "Winning configuration for %d:%!" n ;
          print_configuration_of_micheline_list n l ;
          res
        with
        | DecodeError (se, msg) ->
            print_decode_error se msg ;
            aux ((get_configuration l, (se, msg)) :: errs) ls
        | _ ->
            output_debug @@ sprintf "Unexpected error" ;
            aux errs ls)
  in
  aux [] @@ findkposts (n - 1) l


let tuple2_decode adec bdec = function
  | Mprim {prim = `Pair; args = [m1;m2]; _} | Mseq [m1;m2] ->
     let d1 = adec m1 in
     let d2 = bdec m2 in
     (d1,d2)
  | Mprim {prim = `Pair; args = m1::m; _} | Mseq (m1::m) ->
     let d1 = adec m1 in
     let d2 = tuple1_decode bdec (Mseq m) in
     d1,d2
  | expr -> fail_on_micheline "Invalid tuple2" expr


let tuple2_micheline a_mich b_mich =
  Mprim {prim = `pair; args = [a_mich; b_mich]; annots = []}


let michelson_pair_encode a_encode b_encode = tuple2_encode a_encode b_encode

let michelson_pair_decode adec bdec = tuple2_decode adec bdec

let michelson_pair_micheline = tuple2_micheline

let nat_encode (x : nat) =
  if x >= Z.zero then Mint x else failwith "nats are supposed to be nonnegative"

let nat_decode = function
  | Mint a as m ->
      if Z.(a < zero) then
        fail_on_micheline "Negative number intended to be a nat" m
      else a
  | m -> raise (DecodeError (Micheline m, "Not a nat"))

let nat_micheline = Tzfunc.Proto.prim `nat

let int_michelson_encode (x : int_michelson) = Mint x

let int_michelson_decode = function Mint x -> x | m -> fail_on_micheline "Not an int" m

let int_michelson_micheline = Tzfunc.Proto.prim `int

let tez_encode t = Mint t (* Mprim {prim=`mutez;args =[Mint t];annots=[]} *)

let tez_decode = function
  | Mint t -> t (* Mprim {prim=`mutez;args =[Mint t];annots=[]} -> t *)
  | m -> fail_on_micheline "Not a tez" m

let tez_micheline = Tzfunc.Proto.prim `mutez

let address_encode (a : address) = Mstring a

let address_decode : micheline -> address = function
  | Mstring s -> s
  | Mbytes b as m -> (
      match
        Tzfunc.Binary.Reader.(contract {s = Crypto.hex_to_raw b; offset = 0})
      with
      | Ok (s, _) -> s
      | Error _ -> fail_on_micheline "Not an address" m)
  | m -> fail_on_micheline "Not an address" m

let address_micheline = Tzfunc.Proto.prim `address

let never_type_encode (_ : never_type) = raise (EncodeError("Can't build a Micheline value of type never"))

let never_type_decode m = raise (DecodeError(Micheline m,"Can't decode a value of type never (such a value does not exist)"))

let never_type_micheline = Tzfunc.Proto.prim `never

let timestamp_encode (t : timestamp) =
  (* Mprim {prim = `timestamp; args = [Mint t];annots=[]} *) Mstring t

let timestamp_decode = function
  (* | Mprim {prim = `timestamp; args = [Mint t];annots=[]} -> t *)
  | Mint i -> Tzfunc.Proto.A.(cal_to_str @@ cal_of_str @@ Z.to_string i)
  | Mstring t -> (t : timestamp)
  | m -> fail_on_micheline "Not a timestamp" m

let timestamp_micheline = Tzfunc.Proto.prim `timestamp

let bool_encode (b : bool) =
  Mprim {prim = (if b then `True else `False); args = []; annots = []}

let bool_decode = function
  | Mprim {prim = `True; args = []; _} -> true
  | Mprim {prim = `False; args = []; _} -> false
  | m -> fail_on_micheline "Not a boolean" m

let bool_micheline = Tzfunc.Proto.prim `bool

let option_encode (x_encode : 'a -> micheline) (x : 'a option) : micheline =
  match x with
  | None -> inject_prim `None
  | Some x -> Mprim {prim = `Some; args = [x_encode x]; annots = []}

let option_decode ?(msg = "<type>") (x_decode : micheline -> 'a) = function
  | Mprim {prim = `None; args = []; _} -> None
  | Mprim {prim = `Some; args = [x]; _} -> Some (x_decode x)
  | m -> fail_on_micheline (Printf.sprintf "Not a %s option" msg) m

let make_or_micheline t1 t2 = Mprim {prim = `OR; args = [t1; t2]; annots = []}

let option_micheline a_micheline =
  Mprim {prim = `option; args = [a_micheline]; annots = []}

let list_encode x_encode l : micheline = Mseq (List.map x_encode l)

let ticket_encode t_encode = function
  | Ticket t -> Mprim {prim = `ticket; args = [t_encode t]; annots = []}

let list_decode ?(msg = "<type>") (x_decode : micheline -> 'a) = function
  | Mseq l -> List.map x_decode l
  | m -> fail_on_micheline (Printf.sprintf "Not a %s list" msg) m

let ticket_decode ?(msg = "<type>") (x_decode : micheline -> 'a) = function
  | Mprim {prim=`ticket; args=[x]; _} -> Ticket (x_decode x)
  | m -> fail_on_micheline (Printf.sprintf "Not a %s ticket" msg) m

let ticket_micheline t_micheline = Mprim {prim=`TICKET; args=[t_micheline]; annots=[]}

let list_micheline a_micheline =
  Mprim {prim = `list; args = [a_micheline]; annots = []}

let set_encode = list_encode

let set_decode = list_decode

let set_micheline a_micheline =
  Mprim {prim = `set; args = [a_micheline]; annots = []}
(* As a reminder for the next function: *)
(* [ { "prim": "Elt",
 *                  "args":
 *                    [ { "string": "tz1id1nnbxUwK1dgJwUra5NEYixK37k6hRri" },
 *                      { "int": "1000" } ] } ] *)

let map_encode a_encode b_encode m =
  Mseq
    (List.map
       (fun (x, y) ->
         Mprim {prim = `Elt; args = [a_encode x; b_encode y]; annots = []})
       m)

let decode_map_elt a_decode b_decode = function
  | Mprim {prim = `Elt; args = [x; y]; annots = []} -> (a_decode x, b_decode y)
  | _ -> failwith "Couldn't decode element if map"

(* let map_decode ?(msg1="<type1>") ?(msg2="<type2>") _a_decode _b_decode m = *)
let map_decode ?(msg1 = "<type1>") ?(msg2 = "<type2>") a_decode b_decode
    (m : micheline) : ('a, 'b) map =
  match m with
  | Mseq l -> List.map (decode_map_elt a_decode b_decode) l
  | m ->
      output_debug @@ Format.asprintf "Not handled yet for decode: type %s %s map " msg1 msg2 ;
      fail_on_micheline "Couldn't decode map" m

(* raise (DecodeError (Micheline m,Format.sprintf "Not handled yet for decode: type %s %s map " msg1 msg2))          (\* TODO *\) *)

let map_micheline k_micheline v_micheline =
  Mprim {prim = `map; args = [k_micheline; v_micheline]; annots = []}

let rec big_map_encode (a_encode : 'a -> micheline) (b_encode : 'b -> micheline)
    (m : ('a, 'b) big_map) : micheline =
  match m with
  | Abstract _i -> big_map_encode a_encode b_encode (Literal [])
  | Literal m ->
      Mseq
        (List.map
           (fun (x, y) ->
             Mprim {prim = `Elt; args = [a_encode x; b_encode y]; annots = []})
           m)

let lambda_encode _a_encode _b_encode (l : ('a, 'b) lambda) =
  match l with
  | Lambda l ->
      Mprim {prim = `LAMBDA; args = [l.from; l.to_; l.body]; annots = []}

let lambda_decode _ _ = function
  | Mprim {prim = `LAMBDA; args = [from; to_; body]; _} ->
      Lambda {from; to_; body}
  | m ->
      (* FIXME/TODO: put the actual type. One way to do this would be
         to help/decorate the call to lambda_decode in the generation
         from infer_entrypoints_ocaml *)
      output_debug ~level:1 @@ Format.asprintf
        "Warning: Lambda Decoding support is incomplete, for now input and \
         output types will be Unit\n\
         %!" ;
      Lambda {from = unit_micheline; to_ = unit_micheline; body = m}
(* In this case the from and to type are inaccurate *)
(* fail_on_micheline "Couldn't decode lambda" m *)

let decode_map_elt a_decode b_decode = function
  | Mprim {prim = `Elt; args = [x; y]; annots = []} -> (a_decode x, b_decode y)
  | _ -> failwith "Couldn't decode element of map"

let big_map_decode ?(msg1 = "<type1>") ?(msg2 = "<type2>") a_decode b_decode
    (m : micheline) : ('a, 'b) big_map =
  match m with
  | Mint i -> Abstract i
  | Mseq l -> Literal (List.map (decode_map_elt a_decode b_decode) l)
  | _ ->
      (* let s = EzEncoding.construct Tzfunc.Proto.(micheline_enc.Encoding.json) m in *)
      (*      Format.eprintf "Not handled yet for decode: type %s %s big_map
       * (%s)
       * %!" msg1 msg2 s; *)
      fail_on_micheline
        (Format.sprintf "Error decoding on type big_map<%s,%s>" msg1 msg2)
        m

let big_map_micheline k_micheline v_micheline =
  Mprim {prim = `big_map; args = [k_micheline; v_micheline]; annots = []}

let lambda_micheline x_micheline y_micheline =
  Mprim {prim = `lambda; args = [x_micheline; y_micheline]; annots = []}

let get_big_map_id (b : ('a, 'b) big_map) =
  match b with
  | Literal _ -> failwith "not an abstract bigmap, can't get an id"
  | Abstract i -> Z.to_int i

let string_encode s = Mstring s

let string_decode = function
  | Mstring s -> s
  | m -> fail_on_micheline "Not a string" m

let string_micheline = Tzfunc.Proto.prim `string

let chain_id_micheline = string_micheline

let signature_encode s = Mstring s

let signature_decode = function
  | Mstring s -> s
  | m -> fail_on_micheline "Not a signature" m

let signature_micheline = Tzfunc.Proto.prim `signature

let chain_id_encode (c : chain_id) = Mstring c

let chain_id_decode = string_decode

let key_hash_encode s = Mstring s

let key_hash_decode = function
  | Mstring s -> s
  | m -> fail_on_micheline "Not a key_hash" m

let key_hash_micheline = Tzfunc.Proto.prim `key_hash

let key_encode s = Mstring s

let key_decode = function
  | Mstring s -> s
  | m -> fail_on_micheline "Not a key" m

let key_micheline = Tzfunc.Proto.prim `key

let bytes_encode b = Mbytes b

let bytes_decode = function
  | Mbytes b -> b
  | m -> fail_on_micheline "Not a bytes" m

let bytes_micheline = Tzfunc.Proto.prim `bytes

let contract_encode _x_encode = function
  | Contract c -> c

let contract_decode _x_decode c =
     Contract c

let contract_micheline c = Mprim {prim = `CONTRACT; args = [c]; annots = []}

let operation_encode op = string_encode op (* TODO *)

let operation_decode op = string_decode op

let operation_micheline = Tzfunc.Proto.prim `operation

(* Generators *)
let chooseFrom (l : (unit -> 'a) list) () : 'a =
  let n = List.length l in
  let k = Random.int n in
  List.nth l k ()

module type StaticGenerator = sig
  type t

  val name : string

  type generator = unit -> t

  val default : generator

  val generator : generator

  val set_generator : generator -> unit

  val set_generator_from_list : t list -> unit

  val set_generator_from_property : ?max:Stdlib.Int.t -> (t -> bool) -> unit
end

module type StaticGeneratorBootstraper = sig
  type t

  val default : unit -> t

  val name : string
end

module StaticGeneratorMaker (M : StaticGeneratorBootstraper) = struct
  type t = M.t

  let name = M.name

  type generator = unit -> t

  let default = M.default

  let hidden_generator = ref (fun () -> M.default ())

  let generator () = !hidden_generator ()

  let set_generator g = hidden_generator := fun () -> g ()

  let set_generator_from_list l =
    hidden_generator := chooseFrom (List.map (fun x () -> x) l)

  let set_generator_from_property ?(max = 10000) p =
    hidden_generator :=
      fun () ->
        let rec aux = function
          | 0 ->
              failwith
                (Format.sprintf
                   "failed to find an element after %d iterations"
                   max)
          | k ->
              let temp = M.default () in
              if p temp then temp else aux (k - 1)
        in
        aux max
end

(* module type UnaryGenerator = sig
 *   (\* parameter *\)
 *   type 'a t
 *   val name : string
 *   type 'a param_generator = unit -> 'a
 *   type 'a generator = 'a param_generator -> unit -> 'a t
 *   val default : 'a generator
 *   val generator : 'a generator
 *   val set_generator : 'a generator -> unit
 *   val set_generator_from_list : 'a t list -> 'a generator
 *   val set_generator_from_property : ('a t -> bool) -> 'a generator
 * end *)

(* module type UnaryGeneratorBootstraper =
 *   sig
 *     type 'a t
 *     val default : (unit -> 'a) -> unit -> 'a t
 *     val name : string
 *   end
 *
 * module UnaryGeneratorMaker (M : UnaryGeneratorBootstraper) (\* : UnaryGenerator *\) =
 *   struct
 *     type 'a t = 'a M.t
 *     let name = M.name
 *     type 'a param_generator = unit -> 'a
 *     type 'a generator = 'a param_generator -> unit -> 'a t
 *     let default = M.default
 *     let hidden_generator = ref
 *                              (fun f () -> M.default f ())
 *     let generator f : unit -> 'a t = !hidden_generator f
 *     let set_generator g = hidden_generator := fun gen_a () -> g gen_a ()
 *     let set_generator_from_list l =
 *       hidden_generator := fun _ -> chooseFrom (List.map (fun x -> fun () -> x) l)
 *     let set_generator_from_property ?(max=10000) p =
 *       hidden_generator := fun gen_a () ->
 *       let rec aux = function
 *         | 0 -> failwith (Format.sprintf "failed to find an element after %d iterations" max)
 *         | k -> let temp = M.default gen_a () in if p temp then temp else aux (k-1) in
 *       aux max
 *   end *)

(* module type BinaryGenerator = sig
 *   (\* parameter *\)
 *   type ('a,'b) t
 *   type 'a param_generator = unit -> 'a
 *   type ('a,'b) generator = 'a param_generator -> 'b param_generator -> unit -> ('a,'b) t
 *   val default : ('a,'b) generator
 *   val generator : ('a,'b) generator
 *   val set_generator : ('a,'b) generator -> unit
 *   val set_generator_from_list :  ('a,'b) t list -> ('a,'b) generator
 *   val set_generator_from_property :  (('a,'b)t -> bool) -> ('a,'b) generator
 * end *)

(* module type BinaryGeneratorBootstraper =
 *   sig
 *     type ('a,'b) t
 *     val default : (unit -> 'a) -> (unit -> 'b) -> unit -> ('a,'b) t
 *     val name : string
 *   end
 *
 * module BinaryGeneratorMaker (M : BinaryGeneratorBootstraper) (\* : UnaryGenerator *\) =
 *   struct
 *     type ('a,'b) t = ('a,'b) M.t
 *     let name = M.name
 *     type 'a param_generator = unit -> 'a
 *     type ('a,'b) generator = 'a param_generator -> 'b param_generator -> unit -> ('a,'b) t
 *     let default = M.default
 *     let hidden_generator = ref
 *                              (fun f g () -> M.default f g ())
 *     let generator f g : unit -> ('a,'b) t = !hidden_generator f g
 *     let set_generator g = hidden_generator := fun gen_a gen_b () -> g gen_a gen_b ()
 *     let set_generator_from_list l =
 *       hidden_generator := fun _ _ -> chooseFrom (List.map (fun x -> fun () -> x) l)
 *     let set_generator_from_property ?(max=10000) p =
 *       hidden_generator := fun gen_a gen_b () ->
 *       let rec aux = function
 *         | 0 -> failwith (Format.sprintf "failed to find an element after %d iterations" max)
 *         | k -> let temp = M.default gen_a gen_b () in if p temp then temp else aux (k-1) in
 *       aux max
 *   end *)

let unit_generator () : unit = ()

let default_string_generator () : string =
  chooseFrom
    (List.map
       (fun x () -> x)
       [
         "Non, je tombe d'accord de tout ce qu'il vous plaît,";
         "Tout marche par cabale, et par pur intérêt;";
         "Ce n'est plus que la ruse, aujourd'hui, qui l'emporte,";
         "Et les hommes devraient être faits d'autre sorte.";
         "Mais est-ce une raison, que leur peu d'équité,";
         "Pour vouloir se tirer de leur société?";
         "Tous ces défauts humains nous donnent, dans la vie,";
         "Des moyens d'exercer notre philosophie,";
         "C'est le plus bel emploi que trouve la vertu;";
         "Et si, de probité, tout était revêtu,";
         "Si tous les cœurs étaient, francs, justes, et dociles,";
         "La plupart des vertus nous seraient inutiles,";
         "Puisqu'on en met l'usage à pouvoir, sans ennui,";
         "Supporter dans nos droits, l'injustice d'autrui:";
         "Et de même qu'un cœur, d'une vertu profonde...";
       ])
    ()

module StringGenerator = StaticGeneratorMaker (struct
  type t = string

  let default = default_string_generator

  let name = "string"
end)

let string_generator = StringGenerator.generator

(* Base int means: OCaml standard ints *)
module BaseIntGenerator = StaticGeneratorMaker (struct
  type t = Stdlib.Int.t

  let default () = Random.int 10

  let name = "int"
end)

let baseint_generator = BaseIntGenerator.generator

module IntMichelsonGenerator = StaticGeneratorMaker (struct
  type t = Z.t

  let default () = Z.of_int64 (Random.int64 (Int64.of_int 10))

  let name = "Z"
end)

let int_michelson_generator = IntMichelsonGenerator.generator

let set_baseint_generator ~pos ~max =
  BaseIntGenerator.set_generator @@ fun () ->
  let res = Random.int max in
  if (not pos) && Random.bool () then -1 * res else res

let set_int_generator ~pos ~max =
  IntMichelsonGenerator.set_generator @@ fun () ->
  let res = Z.of_int64 (Random.int64 max) in
  if (not pos) && Random.bool () then Z.mul (Z.neg Z.one) res else res

let default_address_generator () : address =
  chooseFrom
    [
      (fun () -> "tz1id1nnbxUwK1dgJwUra5NEYixK37k6hRri");
      (fun () -> "tz1Tny451rJY5vqUs7JHoYpJHdMH5znNxuxw");
      (fun () -> "tz1Zebr6mNxTPTsbfkvNjAJTmaxGMFbqmhc9");
      (fun () -> "tz1Tk1uqycCoHKNvUPMrgoQMQ2EdazywdVaB");
    ]
    ()

module AddressGenerator = StaticGeneratorMaker (struct
  type t = address

  let default = default_address_generator

  let name = "address"
end)

let address_generator = AddressGenerator.generator

let default_bool_generator () : bool = Random.bool ()

module BoolGenerator = StaticGeneratorMaker (struct
  type t = bool

  let default = default_bool_generator

  let name = "bool"
end)

let bool_generator = BoolGenerator.generator

let default_timestamp_generator () : timestamp = "2021-06-03T09:06:14.990-00:00"

module TimestampGenerator = StaticGeneratorMaker (struct
  type t = timestamp

  let default = default_timestamp_generator

  let name = "timestamp"
end)

let timestamp_generator = TimestampGenerator.generator

let z_generator () : Z.t = int_michelson_generator ()

(* let tez_generator () : dun = z_generator () *)
let default_nat_generator () : nat = Z.abs (z_generator ())

module NatGenerator = StaticGeneratorMaker (struct
  type t = tez

  let default = default_nat_generator

  let name = "nat"
end)

let nat_generator = NatGenerator.generator

let default_tez_generator () = default_nat_generator ()

module TezGenerator = StaticGeneratorMaker (struct
  type t = tez

  let default = default_tez_generator

  let name = "tez"
end)

let tez_generator = TezGenerator.generator

(* Is this necessary? At least it's there now *)
let never_type_generator () = chooseFrom [] ()

let tuple2_generator ex1 ex2 () = (ex1 (), ex2 ())

(* let tuple3_generator ex1 ex2 ex3 () = (ex1 (), ex2 (), ex3 ())
 *
 * let tuple4_generator ex1 ex2 ex3 ex4 () = (ex1 (), ex2 (), ex3 (), ex4 ()) *)

let michelson_pair_generator = tuple2_generator

let make_list f k () =
  let rec aux res = function 0 -> res | i -> aux (f () :: res) (i - 1) in
  aux [] k

module MapSizeGenerator = StaticGeneratorMaker (struct
  type t = BaseIntGenerator.t

  let default () = Random.int 10

  let name = "map size"
end)

let map_size_generator = MapSizeGenerator.generator

let generic_map_generator ~size_gen f g () =
  let sort_uniq_by_keys : ('a * 'b) list -> ('a * 'b) list =
    List.sort_uniq (fun (k1, _v1) (k2, _v2) -> compare k1 k2)
  in
  sort_uniq_by_keys @@ make_list (tuple2_generator f g) (size_gen ()) ()

let map_generator f g = generic_map_generator ~size_gen:map_size_generator f g

module BigMapSizeGenerator = StaticGeneratorMaker (struct
  type t = BaseIntGenerator.t

  let default () = Random.int 10

  let name = "map size"
end)

let big_map_size_generator = BigMapSizeGenerator.generator

let big_map_generator aenc benc =
  chooseFrom
    [
      (fun () ->
        Literal
          (generic_map_generator ~size_gen:big_map_size_generator aenc benc ()));
      (fun () -> Abstract (int_michelson_generator ()));
    ]

let default_operation_generator () : operation =
  string_generator () (* TODO: IMPROVE *)

module OperationGenerator = StaticGeneratorMaker (struct
  type t = operation

  let default () = default_operation_generator ()

  let name = "operation"
end)

let operation_generator = OperationGenerator.generator

module SetSizeGenerator = StaticGeneratorMaker (struct
  type t = BaseIntGenerator.t

  let default () = Random.int 10

  let name = "set size"
end)

let set_size_generator = SetSizeGenerator.generator

module ListSizeGenerator = StaticGeneratorMaker (struct
  type t = BaseIntGenerator.t

  let default () = Random.int 10

  let name = "list size"
end)

let list_size_generator = ListSizeGenerator.generator

let set_generator f = make_list f (set_size_generator ())

let list_generator f = make_list f (list_size_generator ())

let ticket_generator f () : 'a ticket =
  Ticket (f())

let default_bytes_generator () : bytes =
  Crypto.H.mk @@ Bytes.to_string
  @@ Bytes.make
       (baseint_generator () * 2)
       'f' (* output type can't be named either *)
  (* the generated number is multiplied by 2 due to a bug with bytes. Think to
    devide this number in case you change the limit of the generated number. *)

module BytesGenerator = StaticGeneratorMaker (struct
  type t = bytes

  let default = default_bytes_generator

  let name = "bytes"
end)

let bytes_generator = BytesGenerator.generator

(* let address_generator () : address = "tz1id1nnbxUwK1dgJwUra5NEYixK37k6hRri" *)
let option_generator f () =
  chooseFrom [(fun () -> None); (fun () -> Some (f ()))] ()

let lambda_generator _ _ : unit -> ('a, 'b) lambda =
 fun () ->
  Lambda
    {
      from = make_unit ();
      to_ = make_unit ();
      body = Mprim {prim = `FAILWITH; args = []; annots = []};
    }

let contract_generator () : contract = Contract unit_micheline

let default_key_hash_generator () = string_generator ()

module KeyHashGenerator = StaticGeneratorMaker (struct
  type t = key_hash

  let default = default_key_hash_generator

  let name = "key_hash"
end)

let key_hash_generator = KeyHashGenerator.generator

let default_chain_id_generator = chooseFrom [(fun () -> "NetXynUjJNZm7wi");(fun () -> "0x7a06a770")]

module ChainIdGenerator = StaticGeneratorMaker (struct
  type t = chain_id

  let default = default_chain_id_generator

  let name = "chain_id"
end)

let chain_id_generator = ChainIdGenerator.generator

let default_key_generator () = string_generator ()

module KeyGenerator = StaticGeneratorMaker (struct
  type t = key

  let default = default_key_generator

  let name = "key"
end)

let key_generator = KeyGenerator.generator

let default_signature_generator () = string_generator ()

module SignatureGenerator = StaticGeneratorMaker (struct
  type t = signature

  let default = default_signature_generator

  let name = "signature"
end)

let signature_generator = SignatureGenerator.generator

                 

let tuple3_generator ex1 ex2 ex3 () = (ex1 (),ex2 (),ex3 ())

let tuple4_generator ex1 ex2 ex3 ex4 () = (ex1 (),ex2 (),ex3 (),ex4 ())

let tuple5_generator ex1 ex2 ex3 ex4 ex5 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 ())

let tuple6_generator ex1 ex2 ex3 ex4 ex5 ex6 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 ())

let tuple7_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 ())

let tuple8_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 ())

let tuple9_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 ())

let tuple10_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 ())

let tuple11_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 ())

let tuple12_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 ())

let tuple13_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 ())

let tuple14_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 ())

let tuple15_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 ())

let tuple16_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 ())

let tuple17_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 ())

let tuple18_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 ())

let tuple19_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 ())

let tuple20_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 ())

let tuple21_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 ex21 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 (),ex21 ())

let tuple22_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 ex21 ex22 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 (),ex21 (),ex22 ())

let tuple23_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 ex21 ex22 ex23 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 (),ex21 (),ex22 (),ex23 ())

let tuple24_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 ex21 ex22 ex23 ex24 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 (),ex21 (),ex22 (),ex23 (),ex24 ())

let tuple25_generator ex1 ex2 ex3 ex4 ex5 ex6 ex7 ex8 ex9 ex10 ex11 ex12 ex13 ex14 ex15 ex16 ex17 ex18 ex19 ex20 ex21 ex22 ex23 ex24 ex25 () = (ex1 (),ex2 (),ex3 (),ex4 (),ex5 (),ex6 (),ex7 (),ex8 (),ex9 (),ex10 (),ex11 (),ex12 (),ex13 (),ex14 (),ex15 (),ex16 (),ex17 (),ex18 (),ex19 (),ex20 (),ex21 (),ex22 (),ex23 (),ex24 (),ex25 ())

let tuple3_encode enc1 enc2 enc3 (x1,x2,x3) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3];annots=[]}

let tuple4_encode enc1 enc2 enc3 enc4 (x1,x2,x3,x4) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4];annots=[]}

let tuple5_encode enc1 enc2 enc3 enc4 enc5 (x1,x2,x3,x4,x5) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5];annots=[]}

let tuple6_encode enc1 enc2 enc3 enc4 enc5 enc6 (x1,x2,x3,x4,x5,x6) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6];annots=[]}

let tuple7_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 (x1,x2,x3,x4,x5,x6,x7) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7];annots=[]}

let tuple8_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 (x1,x2,x3,x4,x5,x6,x7,x8) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8];annots=[]}

let tuple9_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 (x1,x2,x3,x4,x5,x6,x7,x8,x9) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9];annots=[]}

let tuple10_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10];annots=[]}

let tuple11_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11];annots=[]}

let tuple12_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12];annots=[]}

let tuple13_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13];annots=[]}

let tuple14_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14];annots=[]}

let tuple15_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15];annots=[]}

let tuple16_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16];annots=[]}

let tuple17_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17];annots=[]}

let tuple18_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18];annots=[]}

let tuple19_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19];annots=[]}

let tuple20_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20];annots=[]}

let tuple21_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 enc21 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20;enc21 x21];annots=[]}

let tuple22_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 enc21 enc22 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20;enc21 x21;enc22 x22];annots=[]}

let tuple23_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 enc21 enc22 enc23 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20;enc21 x21;enc22 x22;enc23 x23];annots=[]}

let tuple24_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 enc21 enc22 enc23 enc24 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20;enc21 x21;enc22 x22;enc23 x23;enc24 x24];annots=[]}

let tuple25_encode enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 enc9 enc10 enc11 enc12 enc13 enc14 enc15 enc16 enc17 enc18 enc19 enc20 enc21 enc22 enc23 enc24 enc25 (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25) = Mprim {prim = `Pair;args=[enc1 x1;enc2 x2;enc3 x3;enc4 x4;enc5 x5;enc6 x6;enc7 x7;enc8 x8;enc9 x9;enc10 x10;enc11 x11;enc12 x12;enc13 x13;enc14 x14;enc15 x15;enc16 x16;enc17 x17;enc18 x18;enc19 x19;enc20 x20;enc21 x21;enc22 x22;enc23 x23;enc24 x24;enc25 x25];annots=[]}
let tuple3_decode dec1 dec2 dec3 =
     let ntuple_to_Sntuple =
fun x (x1,x2) -> (x,x1,x2) in
     function
  | Mprim {prim = `Pair; args = [m1;m2]; _} | Mseq [m1;m2] ->
     let d1 = dec1 m1 in
     let d2 = tuple2_decode dec2 dec3 (Mseq [m2]) in
     ntuple_to_Sntuple d1 d2
  | Mprim {prim = `Pair; args = m1::m; _} | Mseq (m1::m) ->
     let d1 = dec1 m1 in
     let d2 = tuple2_decode dec2 dec3 (Mseq m) in
     ntuple_to_Sntuple d1 d2
  | expr -> fail_on_micheline "Invalid tuple2" expr

let tuple4_decode dec1 dec2 dec3 dec4 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4)| expr -> fail_on_micheline "Invalid tuple4" expr

let tuple5_decode dec1 dec2 dec3 dec4 dec5 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5)| expr -> fail_on_micheline "Invalid tuple5" expr

let tuple6_decode dec1 dec2 dec3 dec4 dec5 dec6 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6)| expr -> fail_on_micheline "Invalid tuple6" expr

let tuple7_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7)| expr -> fail_on_micheline "Invalid tuple7" expr

let tuple8_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8)| expr -> fail_on_micheline "Invalid tuple8" expr

let tuple9_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9)| expr -> fail_on_micheline "Invalid tuple9" expr

let tuple10_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10)| expr -> fail_on_micheline "Invalid tuple10" expr

let tuple11_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11)| expr -> fail_on_micheline "Invalid tuple11" expr

let tuple12_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12)| expr -> fail_on_micheline "Invalid tuple12" expr

let tuple13_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13)| expr -> fail_on_micheline "Invalid tuple13" expr

let tuple14_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14)| expr -> fail_on_micheline "Invalid tuple14" expr

let tuple15_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15)| expr -> fail_on_micheline "Invalid tuple15" expr

let tuple16_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16)| expr -> fail_on_micheline "Invalid tuple16" expr

let tuple17_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17)| expr -> fail_on_micheline "Invalid tuple17" expr

let tuple18_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18)| expr -> fail_on_micheline "Invalid tuple18" expr

let tuple19_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19)| expr -> fail_on_micheline "Invalid tuple19" expr

let tuple20_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20)| expr -> fail_on_micheline "Invalid tuple20" expr

let tuple21_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 dec21 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20;x21];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20,dec21 x21)| expr -> fail_on_micheline "Invalid tuple21" expr

let tuple22_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 dec21 dec22 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20;x21;x22];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20,dec21 x21,dec22 x22)| expr -> fail_on_micheline "Invalid tuple22" expr

let tuple23_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 dec21 dec22 dec23 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20;x21;x22;x23];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20,dec21 x21,dec22 x22,dec23 x23)| expr -> fail_on_micheline "Invalid tuple23" expr

let tuple24_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 dec21 dec22 dec23 dec24 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20;x21;x22;x23;x24];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20,dec21 x21,dec22 x22,dec23 x23,dec24 x24)| expr -> fail_on_micheline "Invalid tuple24" expr

let tuple25_decode dec1 dec2 dec3 dec4 dec5 dec6 dec7 dec8 dec9 dec10 dec11 dec12 dec13 dec14 dec15 dec16 dec17 dec18 dec19 dec20 dec21 dec22 dec23 dec24 dec25 = function
          | Mprim {prim = `Pair;args=[x1;x2;x3;x4;x5;x6;x7;x8;x9;x10;x11;x12;x13;x14;x15;x16;x17;x18;x19;x20;x21;x22;x23;x24;x25];annots=[]} -> (dec1 x1,dec2 x2,dec3 x3,dec4 x4,dec5 x5,dec6 x6,dec7 x7,dec8 x8,dec9 x9,dec10 x10,dec11 x11,dec12 x12,dec13 x13,dec14 x14,dec15 x15,dec16 x16,dec17 x17,dec18 x18,dec19 x19,dec20 x20,dec21 x21,dec22 x22,dec23 x23,dec24 x24,dec25 x25)| expr -> fail_on_micheline "Invalid tuple25" expr
let tuple3_micheline mich1 mich2 mich3 = Mprim {prim = `pair; args = [mich1;mich2;mich3]; annots = []}

let tuple4_micheline mich1 mich2 mich3 mich4 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4]; annots = []}

let tuple5_micheline mich1 mich2 mich3 mich4 mich5 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5]; annots = []}

let tuple6_micheline mich1 mich2 mich3 mich4 mich5 mich6 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6]; annots = []}

let tuple7_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7]; annots = []}

let tuple8_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8]; annots = []}

let tuple9_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9]; annots = []}

let tuple10_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10]; annots = []}

let tuple11_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11]; annots = []}

let tuple12_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12]; annots = []}

let tuple13_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13]; annots = []}

let tuple14_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14]; annots = []}

let tuple15_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15]; annots = []}

let tuple16_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16]; annots = []}

let tuple17_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17]; annots = []}

let tuple18_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18]; annots = []}

let tuple19_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19]; annots = []}

let tuple20_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20]; annots = []}

let tuple21_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 mich21 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20;mich21]; annots = []}

let tuple22_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 mich21 mich22 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20;mich21;mich22]; annots = []}

let tuple23_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 mich21 mich22 mich23 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20;mich21;mich22;mich23]; annots = []}

let tuple24_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 mich21 mich22 mich23 mich24 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20;mich21;mich22;mich23;mich24]; annots = []}

let tuple25_micheline mich1 mich2 mich3 mich4 mich5 mich6 mich7 mich8 mich9 mich10 mich11 mich12 mich13 mich14 mich15 mich16 mich17 mich18 mich19 mich20 mich21 mich22 mich23 mich24 mich25 = Mprim {prim = `pair; args = [mich1;mich2;mich3;mich4;mich5;mich6;mich7;mich8;mich9;mich10;mich11;mich12;mich13;mich14;mich15;mich16;mich17;mich18;mich19;mich20;mich21;mich22;mich23;mich24;mich25]; annots = []}

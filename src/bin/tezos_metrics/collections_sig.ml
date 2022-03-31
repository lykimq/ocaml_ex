(* This file declare type t for metrics.ml 
   Type t is the type of a metric exatractor. *)

type t = 
  | Extractor : {
    name: string;
    init: namespace:string -> string -> 's;
    update: 's -> unit Lwt.t
  } -> t

(* module type S is the signature of a set of metrics for a subsytem *)

module type S = sig 
  val subsystem : string

  val all: t list 

end

(* The signature for a generic metric *)

module type T = sig
  (* define module Config is a Config_sig.S *)
  module Config : Config_sig.S 

  val name: string
  val subsystem: string 
  val help: string 
  val path: string
  val delay : float
  val member : string option

end

(* U is used in collections 
  It is the signature of the extractor functions for a generic metric 
*)

module type U = sig 

  type t 

  val init : namespace:string -> string -> t 
  val set : t -> string -> unit
end 

(* the signature of the extractor functions for a metric using a command *)

module type V = sig 
  include U
  val cmd : string * string array
end 
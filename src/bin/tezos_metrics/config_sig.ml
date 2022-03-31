(* Config_sig is using in collections_sig *)

module type S = sig
  val uri : Uri.t

  val chain : bool

  val delay : float 
  
  val data_dir : string
   
end

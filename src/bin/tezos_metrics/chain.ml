(* it is called in metrics.ml Chain.Make *)


module HeadMetric = struct

  type t = {
    level : Prometheus.Gauge.t; 
    cycle : Prometheus.Gauge.t
    }

    let decode s =
      let json = Yojson.Safe.from_string s in 
      let level = Yojson.Safe.Util.(to_int @@ member "level" json) in 
      let cycle = Yojson.Safe.Util.(to_int @@ member "cycle" json) in 
      (level, cycle)

    let init ~namespace name =
      let g name =
        Prometheus.Gauge.v 
        ~namespace
        ~help:"Main chain current level and cycle"
        ~subsystem:"chain"
        name
      in 
      {
        level = g (name ^ "_level"); 
        cycle = g (name ^ "_cycle")
      }

    let set store s =
      let (level, cycle) = decode s in 
      Prometheus.Gauge.set store.level (float level);
      Prometheus.Gauge.set store.cycle (float cycle)

end

module MakeHeadMetric (M: Collections_sig.T) =
  Collections.MakeRpcMetric (M) (HeadMetric)

module Make (Config : Config_sig.S): Collections_sig.S = struct 
  
  let subsystem = "chain"
  let delay = 45.

  module Head = MakeHeadMetric (struct
    module Config = Config

    let name = "current"

    let path = "/chain/main/blocks/head/helpers/current_level"

    let member = None 

    let help = "Level and cycles associated with the current head"

    let subsystem = subsystem

    let delay = delay

  end )


  let all = [Head.l]

end
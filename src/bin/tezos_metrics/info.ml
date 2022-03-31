(* info is using in metrics for the  field info  *)


module VersionMetrics = struct

  type t = {version: Prometheus.Gauge.family}

  let decode s =
    let json = Yojson.Safe.from_string s in 
    let version =
      let v = Yojson.Safe.Util.(member "version" json) in
      let major = Yojson.Safe.Util.(v |> member "major" |> to_int) in 
      let minor = Yojson.Safe.Util.(v |> member "minor" |> to_int) in
      let info =
        Yojson.Safe.Util.(v |> member "additional_info" |> to_assoc)
      in
      let pp_info fmt info =
        Format.pp_print_list (fun fmt (s,json) -> Format.fprintf fmt "%s-%d" s (Yojson.Safe.Util.to_int json)) fmt info
      in
      Format.asprintf "%i.%i.%a" major minor pp_info info
    in 
    let network_version =       
      Yojson.Safe.Util.(json |> member "network_version")     
    in
    let chain_name =
      Yojson.Safe.Util.(network_version |> member "chain_name" |> to_string)   
    in
    [
      version;
      chain_name
    ]

  let init ~namespace name =
    {
      version = 
      Prometheus.Gauge.v_labels
      ~namespace
      ~label_names:["version"; "chain_name"]
      ~help:"Node version"
      ~subsystem:"info"
      name;
    }

    let set store s =
      let labels = decode s in 
      let t = Prometheus.Gauge.labels store.version labels in 
      Prometheus.Gauge.set t 1.

end

module MakeVersionMetric (M: Collections_sig.T) = 
  Collections.MakeRpcMetric (M) (VersionMetrics)

module Make (Config: Config_sig.S) : Collections_sig.S = struct

  let subsystem = "info"

  let delay = 3600.

  module Version = MakeVersionMetric (struct
    module Config = Config

    let name = "version"
    let help = "info about node"
    let subsystem = subsystem
    let delay = delay 
    let path = "/version"
    let member = None 

  end)

  let all = [Version.l]


end 
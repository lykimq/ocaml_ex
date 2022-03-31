(* Define metrics for test_tezos_prometheus.
   It is a module Metrics in the file `test_prometheus.ml` 
*)

type t = (module Collections_sig.S)

let concretize_collections ~namespace ~(collections: t list) =
  List.concat
  (List.map (fun collection -> 
    let (module C: Collections_sig.S) = collection in 
    List.map (fun (Collections_sig.Extractor {init; name; update}) ->
      let store = init ~namespace name in 
      update store) C.all) collections)

let all config =
  let (module Config: Config_sig.S) = config in 
  let collections =
    let info : t = (module Info.Make (Config)) in 
    let l = ref [info] in 
    (* It is using Logs to log the metrics information *)
    if Config.chain then 
      let chain : t = (module Chain.Make (Config)) in 
    l := chain :: !l 
    else 
      Logs.info (fun m -> m "Disable chain metrics");
    if String.length Config.data_dir > 0 then 
      let storage : t = (module Storage.MakeStorage (Config)) in 
      l := storage :: !l
   else 
    Logs.info (fun m -> m "--data-dir not provided. Disable storage metrics");
   !l
  in 
  concretize_collections ~namespace:"tezos_metrics" ~collections
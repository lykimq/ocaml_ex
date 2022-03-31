(* storage in metrics for data_dir *)

module MakeStorage (Config: Config_sig.S) : Collections_sig.S = struct
  let delay = 5.

  let subsystem = "storage"

  module Total = Collections.MakeDirSize (struct
    module Config = Config 

    let name = "total"
    let help = "Total size of the Tezos data directory"

    let member = None 
    let path = ""
    let subsystem = subsystem
    let delay = delay
  end)

  let all = [Total.l]

end
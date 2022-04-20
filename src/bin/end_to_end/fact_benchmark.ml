

let fact_benchmarks : unit -> Pipeline.goal =
  fun () ->
    (* setup benchmark for fa12 contract *)
    let (b, op) = Fa12_benchmarks.set_up_fa12 () in 
    (* originate the contract *)
    let (b, x) =
    (b, op >>= fun (_, (_ ,alice, _)) ->
      let fact_contract = read_file ".tz" in 
      let initial_storage = "0" in 
      Origination
      {
        originator = alice;
        amount = 0;
        contract = fact_contract;
        initial_storage
      }
      >>| fun (b, fact) -> (b, alice, fact))
  in 
  (b, x) >>=! fun (_, (_, alice, fact)) ->
  Transfer {
    sender = alice;
    recipient = fact;
    amount = 1_000_000;
    parameters="100"
  }
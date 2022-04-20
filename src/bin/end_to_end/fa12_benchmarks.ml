let set_up_fa12 : unit -> Block.t * (Contract.t * Contract.t * Contract.t) Pipeline.operation =
  fun () ->
    (* initial the context and contract *)
    let (b, contracts) = Context.init 2 |> force_global_lwt in 
    let alice = List.nth contracts 0 |> Option.get in 
    let bob = List.nth contracts 1 |> Option.get in 
    (* initial storage for the contract *)
    let initial_storage =
      sprintf 
      {|Pair {Elt "%s" (Pair {} 100000000000000)} 100000000000000|}
      (Contract.to_b58check alice)
    in 
    let pipeline = 
      Origination 
      {
        originator = alice;
        amount = 100;
        contract = read_file "./contracts/fa1.2.tz";
        initial_storage
      }
    in 
    (b, pipeline)
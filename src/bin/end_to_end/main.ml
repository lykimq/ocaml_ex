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
  

let fact_benchmarks : unit -> Pipeline.goal =
  fun () ->
    (* setup benchmark for fa12 contract *)
    let (b, op) = set_up_fa12 () in 
    (* originate the contract *)
    let (b, x) =
    (b, op >>= fun (_, (_ ,alice, _)) ->
      let fact_contract = read_file "./contracts/fact.tz" in 
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
  (* transfer with the originated contract *)
  Transfer {
    sender = alice;
    recipient = fact;
    amount = 1_000_000;
    parameters="100"
  }


let create_benchmark name f =
  let (eval, closure) = f () in  
  Core_bench.Bench.Test.create ~name (fun () -> Lwt_main.run @@ closure ())

let benchmarks =
  [
    (* benchmarks the transfer the contract fact *)
    create_benchmark "Fact" fact_benchmarks
  ]

let () =
  Core.Command.run (Core_bench.Bench.make_command benchmarks)
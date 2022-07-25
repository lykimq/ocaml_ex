open Mligo

type storage = int
[@@store]

type params = Propose of int
[@@entry Main]

let rec sum (acc : int) (n : int) (i : int) : int =
  if i > n / 2 then
    acc
  else if n mod i = 0n then
    sum (i + acc) n (i + 1)
  else
    sum acc n (i + 1)

let perfect_number (n : int) =
  sum 0 n 1 = n

let play (number, store : int * storage) =
  if store < number then
    if perfect_number number then
      let source : (unit, storage) contract =
        let source : address = Tezos.get_source None in
        Tezos.get_contract_with_error None source "Source address not found." in
      let transaction : operation =
        Tezos.transaction None unit 2000u source in
      ([transaction], number : operation list * storage)
    else
      (failwith "The number you provided it not a perfect number." : operation list * storage)
  else
    (failwith "The number you provided is smaller than the current perfect number." : operation list * storage)

let main (action, store : params * storage) : operation list * storage =
  match action with
  | Propose number -> play (number, store)

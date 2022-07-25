#if !PERFECT_NUMBER
#define PERFECT_NUMBER

type storage = int

type params =
  | Propose of int 

let rec sum (acc : int) (n : int) (i : int) : int =
  if i > (n / 2)
  then acc
  else if (n mod i) = 0n then sum (i + acc) n (i + 1) else sum acc n (i + 1)

let perfect_number (n : int) =
  (sum 0 n 1) = n

let play ((number, store) : (int * storage)) =
  if store < number
  then
    (if perfect_number number
     then
       let source : unit contract =
         let source : address = Tezos.get_source () in
         Tezos.get_contract_with_error source "Source address not found." in
       let transaction : operation = Tezos.transaction unit 2000mutez source in
       (([transaction], number) : (operation list * storage))
     else
       (failwith "The number you provided it not a perfect number." : (operation
                                                                         list *
                                                                       storage)))
  else
    (failwith
       "The number you provided is smaller than the current perfect number." : 
       (operation list * storage))

let main ((action, store) : (params * storage)) : (operation list * storage) =
  match action with | Propose number -> play (number, store)

#endif
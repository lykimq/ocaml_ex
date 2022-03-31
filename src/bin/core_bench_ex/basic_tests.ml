
open Core_bench

let t1 = Bench.Test.create ~name:"Id" (fun () -> ())

let tests = [t1]

let command = Bench.make_command tests
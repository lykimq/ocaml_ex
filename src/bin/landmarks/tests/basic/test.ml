let call = Landmark.register "fib"

(*register: ?id:string -> ?location:string -> string -> landmark 
  registers a new landmark. *)
let main = Landmark.register "main"

let rec fib n =
  (* wrap: landmark -> ('a -> 'b) -> 'a -> 'b
    Puts landmark blocks around a function (and close the block and re-raise
    in case of uncaught exception)
  *)
  Landmark.wrap call
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2)) n

let () =
  let open Landmark in
  (* start_profiling: ?profiling_options:profiling_options -> unit -> unit
    starts the profiling

    - profiling_options: unit -> profiling_options (get the options)

    type profiling_options: debug; etc. 
    https://github.com/LexiFi/landmarks/blob/master/src/landmark.mli
  *)
  start_profiling
    ~profiling_options:{default_options with format = JSON; debug = true} ();
  (* enter: landmark -> unit
    Begin a landmark block
  *)
  enter main;
  Printf.printf "Fib 7: %d\n%!" (fib 7);
  (* exit: landmark -> unit
      ends a landmark block
  *)
  exit main;
  (* profiling: unit -> bool 
    checks if the profiling is ongoing
  *)
  if profiling () then begin
    let open Landmark.Graph in
    (* export: ?label:string -> unit -> Graph.graph
      eport the profiling information of the current process
    *)
    let cg = export () in
    (* aggregate_landmarks : graph -> graph 
      computes the quotient by the relation "being an instance of the same 
      landmark"
    *)
    let agg = aggregate_landmarks cg in
    (* nodes: graph -> node list
      returns all nodes of a graph
    *)
    let all_nodes = nodes agg in
    print_endline "\nLandmark reached:";
    all_nodes
    |> List.map (fun {name; _} -> name)
    |> List.sort compare
    |> List.iter print_endline
  end
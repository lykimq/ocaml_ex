open Memtrace.Geometric_sampler
(*
 Geometric sampler is an efficient sampler for geometrically distributed
 random variables
 
 A sample is parametrised by a sampling rate lambda, and simulates a long sequence
 of flips of a biased that has probability lambda of coming up heads.


*)

let test ~sampling_rate =
  (*
    val make: ?rand:Random.State.t -> sampling_rate:float -> unit -> t
    create a sampler with a given sampling rate and randomness source.
    The default for [rand] uses a constant seed, giving determinstic results;
  *)
  let t = make ~sampling_rate () in
  let total = 100_000 in
  let s = ref total in
  let n = ref 0 in
  let n3 = ref 0 in
  let rec go () =
    (* val draw: t -> int
       returns a geometrically-distributed random integer in the range [1 .. inf]
       with mean 1/sampling_rate
    *)
    let k = draw t in
    s := !s - k;
    if !s >= 0 then begin
      incr n;
      if k = 3 then incr n3;
      go ()
    end in
  go ();
  let observed_rate = float_of_int !n /. float_of_int total in
  let expected_p3 =
    (1. -. sampling_rate) *. (1. -. sampling_rate) *. sampling_rate in
  let observed_p3 = float_of_int !n3 /. float_of_int !n in
  Printf.printf "sample rate: %2f (%.2f), P (X=3): %.2f (%.2f) \n" sampling_rate
    observed_rate expected_p3 observed_p3

let () =
  [0.01; 0.13; 0.42; 0.73; 1.] |> List.iter (fun s -> test ~sampling_rate:s)

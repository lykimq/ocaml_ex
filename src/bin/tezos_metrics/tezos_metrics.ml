(* https://gitlab.com/nomadic-labs/tezos-metrics/-/blob/master-mainnet/src/main.ml 

$ dune build
$ dune exec -- ./main.exe --listen-prometheus=9091 (or 9090)
*)


let reporter =
  let report _ level ~over k msgf =
    let with_timestamp ?header ?tags:_ fmt =
      let k' line = 
        Lwt.ignore_result @@ Lwt_io.write_line Lwt_io.stdout line;
        over ();
        k ()
      in
      let open Unix in 
      let t = localtime @@ time () in 
      Format.kasprintf 
      k'
     ("%04d-%02d-%02dT%02d:%02d:%02d %a " ^^ fmt)
     (t.tm_year + 1900)
     (t.tm_mon + 1)
     t.tm_mday
     t.tm_hour
     t.tm_min
     t.tm_sec
     Logs.pp_header
     (level, header)     
    in 
    msgf with_timestamp
  in 
  {Logs.report}

let main () host port delay data_dir nochain prometheus_config =
  let module Config = struct 
    let uri = Uri.make ~scheme:"http" ~host ~port () 

    let delay = delay 

    let data_dir = data_dir

    let chain = not nochain 

  end in 

  let threads = 
    Metrics.all (module Config) @ Prometheus_unix.serve prometheus_config 
  in 
  Logs.set_reporter reporter;
  Logs.info (fun m -> m "Start Tezos metrics");
  Logs.info (fun m -> m "Scraping Tezos node %s" (Uri.to_string Config.uri));
  Logs.info (fun m -> m "Listening to http://localhost:9091");
  Lwt_main.run (Lwt.choose threads)

let setup_log style_renderer level =   
  Fmt_tty.setup_std_outputs ?style_renderer () ;  
  Logs.set_level level ;   
  ()

open Cmdliner 

let setup_log =
  let _env = Cmd.Env.info "TOOL_VERBOSITY" ~doc:"" in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let host =   
  let doc = "Tezos host." in  
  let env = Cmd.Env.info "TEZOS_HOST" ~doc in  
  Arg.(value & opt string "localhost" & info ["h"; "host"] ~env ~docv:"TEZOS_HOST" ~doc)

let port =
  let doc = "Tezos rpc port." in
  let env = Cmd.Env.info "TEZOS_RPC_PORT" ~doc in 
  Arg.(value & opt int 8732 & info ["p"; "port"] ~env ~docv:"TEZOS_HOST" ~doc)

let delay =
  let doc = "Delay interval." in
  Arg.(value & opt float 60.0 & info ["d"; "delay"] ~docv:"DELAY" ~doc)

let data_dir =
  let doc =
    "Tezos data directory. If not set, storage metrics will be disabled"
  in
  let env = Cmd.Env.info "TEZOS_DATA_DIR" ~doc in
  Arg.(
    value & opt string "" & info ["data-dir"] ~env ~docv:"TEZOS_DATA_DIR" ~doc)


let nochain =
  let doc = "Disable chain metrics (active by default)" in
  Arg.(value & flag & info ["no-chain"] ~doc)


let cmd =
  Logs.info (fun f -> f "Tezos-metrics.");
  print_endline "Provides various metrics about the Tezos nodes";
  let spec = Term.(const main $ setup_log $ host $ port $ delay $ data_dir $ nochain $ Prometheus_unix.opts) in 
  let info = Cmd.info "example" ~doc:"Tezos-metrics" in 
  Cmd.v info spec

let () = exit (Cmd.eval cmd)

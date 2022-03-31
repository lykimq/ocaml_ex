(* collections is used in info *)

open Collections_sig

let create ~init ~update name = Extractor {init; name; update}

module MakeRpcMetric (M: T) (N: U) = struct 

    let name = M.name
    let help = M.help
    let subsystem = M.subsystem
    let uri = Uri.with_path M.Config.uri M.path 

    let init ~namespace name = N.init ~namespace name 

    let set store json = N.set store json 
    
    let update store =
      let rec loop delay =
      Lwt.catch
        (fun () ->
          Cohttp_lwt_unix.Client.get uri
          >>= function
          | (resp, body) ->
              let status = Cohttp_lwt_unix.Response.status resp in
              if status == `OK then
                Cohttp_lwt.Body.to_string body
                >>= fun s ->
                Lwt.catch
                  (fun () ->
                    set store s ;
                    Logs.debug (fun m ->
                        m "[%s] tick again in %.0f seconds." name delay) ;
                    Lwt.return delay)
                  (function
                    | (Yojson.Json_error _ | Yojson.Safe.Util.Type_error _) as
                      ex ->
                        Logs.warn (fun m ->
                            m
                              "[%s] while decoding string '%s': got %s."
                              name
                              s
                              (Printexc.to_string ex)) ;
                        Lwt_unix.sleep delay >>= fun () -> loop delay
                    | ex ->
                        Logs.err (fun m ->
                            m "[%s] %s." name (Printexc.to_string ex)) ;
                        Lwt.return 10000000.)
              else
                Cohttp_lwt.Body.to_string body
                >>= fun s ->
                Logs.warn (fun m ->
                    m
                      "[%s] %s : %s"
                      name
                      (Cohttp.Code.string_of_status status)
                      s) ;
                Lwt.return delay)
        (function
          | Unix.Unix_error (Unix.ECONNREFUSED, "connect", _)
          | Unix.Unix_error (Unix.ECONNRESET, "read", _) ->
              let new_d = min 30. (2. *. delay) in
              Logs.warn (fun m ->
                  m
                    "[%s] could not connect to tezos node (%a), trying again in \
                     %.0f seconds."
                    name
                    Uri.pp
                    uri
                    new_d) ;
              Lwt.return new_d
          | ex ->
              Logs.err (fun m -> m "[%s] %s" name (Printexc.to_string ex)) ;
              Lwt.return 10000000.) >>= fun delay -> Lwt_unix.sleep delay >>= fun () -> loop delay
      [@@tailcall]
    in
    loop M.delay >>= fun _ -> Lwt.return_unit


    let l = create ~init ~update name 
end


module MakeCmdMetric (M: T) (N: V) = struct 

  let name = M.name 
  let help = M.help 
  let subsystem = M.subsystem

  let init ~namespace name = N.init ~namespace name 

  let set store s = N.set store s 

  let update st =
    let rec loop () =
      Lwt_process.pread N.cmd
      >>= fun s ->
      Lwt.catch
        (fun () -> Lwt.return (set st s) >>= fun () -> Lwt_unix.sleep M.delay)
        (function
          | End_of_file ->
              Logs.warn (fun m -> m "[%s] file %s: End_of_file@." name s) ;
              Lwt_unix.sleep M.delay
          | ex ->
              Logs.err (fun m ->
                  m "[%s] file %s: %s@." name s (Printexc.to_string ex)) ;
              Lwt_unix.sleep 1000000.)
      >>= fun () -> loop ()
      [@@tailcall]
    in
    loop ()

  let l = create ~init ~update name

end

module MakeDirSize (M: T) =
  MakeCmdMetric (M) (struct
    type t = Prometheus.Gauge.t 

    let b_to_mb x = x / 1024

    let init ~namespace name =
      Prometheus.Gauge.v 
       ~namespace
       ~help:M.help
       ~subsystem:M.subsystem
       name
    
    let cmd =
      (
        "du", 
        [|
          "du";
          "-s";
          "--exclude='*_tmp'";
          Filename.concat M.Config.data_dir M.path |])

    let decode s = Scanf.sscanf s "%d%s" (fun d _      -> b_to_mb d)
    let set store s = Prometheus.Gauge.set store (float (decode s))
end)
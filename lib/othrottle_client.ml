open! Core
open! Async

let stdout_writer = Lazy.force Writer.stdout

let connect_to_socket_at_path socket_path =
  Socket.Address.Unix.create socket_path
  |> Tcp.Where_to_connect.of_unix_address
  |> Rpc.Connection.client
;;

let connect socket_path_opt =
  let open_sockets =
    match socket_path_opt with
    | Some s ->
      (match%map connect_to_socket_at_path s with
       | Ok s -> [ s ]
       | Error _ -> [])
    | None ->
      (match%map
         Othrottle_socket.get_possible_socket_paths ()
         |> List.map ~f:connect_to_socket_at_path
         |> Deferred.all
       with
       | [] -> []
       | a -> List.filter_map a ~f:Result.ok)
  in
  match%bind open_sockets with
  | [] -> return (Error (Error.of_string "Cannot find suitable socket to connect to"))
  | f :: _ -> return (Ok f)
;;

let connect_and_run socket_path ~f =
  match%bind connect socket_path with
  | Ok conn -> f conn
  | Error e -> return (Log.Global.error "%s" (Error.to_string_mach e))
;;

let add_job ~socket_path ~cmds ~origin =
  connect_and_run socket_path ~f:(fun conn ->
    let args : Othrottle_protocol.Job.t = { jobs = cmds; origin } in
    Rpc.Rpc.dispatch_exn Othrottle_protocol.create_job_rpc conn args)
;;

type status_output_format =
  [ `Sexp
  | `Markdown
  ]

let status ~socket_path ~include_finished output_format =
  connect_and_run socket_path ~f:(fun conn ->
    let%bind res =
      Rpc.Rpc.dispatch_exn Othrottle_protocol.status_rpc conn { include_finished }
    in
    return
      (match output_format with
       | `Sexp ->
         Othrottle_protocol.Status.sexp_of_t res
         |> Sexp.to_string_hum
         |> Writer.write stdout_writer
       | `Markdown ->
         Othrottle_protocol.Status.to_markdown_table res |> Writer.write stdout_writer))
;;

let kill_job ~socket_path ~cmd =
  connect_and_run socket_path ~f:(fun conn ->
    let args : Othrottle_protocol.Kill_job_query.t = { cmd } in
    match%map Rpc.Rpc.dispatch_exn Othrottle_protocol.kill_job_rpc conn args with
    | Ok _ -> ()
    | Error x -> Error.to_string_hum x |> Writer.write stdout_writer)
;;

type get_config_output_format =
  [ `Sexp
  | `Toml
  ]

let get_config ~socket_path output_format =
  connect_and_run socket_path ~f:(fun conn ->
    let%bind c = Rpc.Rpc.dispatch_exn Othrottle_protocol.get_config_rpc conn () in
    return
      (match output_format with
       | `Sexp -> Config.sexp_of_t c |> Sexp.to_string_hum |> Writer.write stdout_writer
       | `Toml ->
         Config.otoml_of_t c
         |> Otoml.Printer.to_string ~force_table_arrays:true
         |> Writer.write stdout_writer))
;;

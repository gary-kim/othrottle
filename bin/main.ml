open! Core
open! Async

let start_server socket_path config_path =
  [%log.info "Starting server"];
  match Othrottle.Othrottle_server.init_and_recv socket_path ~config_path with
  | Some _ -> Deferred.never ()
  | None -> Deferred.return (shutdown 0)
;;

let status_cmd =
  Command.async
    ~extract_exn:true
    ~summary:"Get current job status"
    (let%map_open.Command include_finished =
       flag "finished" no_arg ~doc:" include finished job in response"
     and socket_path = flag "socket" (optional string) ~doc:"PATH server socket path"
     and format =
       flag_optional_with_default_doc_string
         "format"
         Othrottle.Othrottle_client.Status_output_format.arg_type
         Othrottle.Othrottle_client.Status_output_format.to_string
         ~default:Othrottle.Othrottle_client.Status_output_format.Sexp
         ~doc:"FORMAT output format of status"
     in
     fun () -> Othrottle.Othrottle_client.status ~socket_path ~include_finished format)
;;

let config_cmd =
  Command.async
    ~extract_exn:true
    ~summary:"Get server configuration"
    (let%map_open.Command format =
       flag_optional_with_default_doc_string
         "format"
         Othrottle.Othrottle_client.Get_config_output_format.arg_type
         Othrottle.Othrottle_client.Get_config_output_format.to_string
         ~default:Othrottle.Othrottle_client.Get_config_output_format.Sexp
         ~doc:"FORMAT output format of config"
     and socket_path = flag "socket" (optional string) ~doc:"PATH server socket path" in
     fun () -> Othrottle.Othrottle_client.get_config ~socket_path format)
;;

let job_cmd =
  Command.async
    ~extract_exn:true
    ~summary:"Add a job"
    (let%map_open.Command origin =
       flag_optional_with_default_doc_string
         "origin"
         string
         Fn.id
         ~default:""
         ~doc:"ORIGIN origin for tracking"
     and cmds = anon (non_empty_sequence_as_list ("jobs" %: string))
     and socket_path = flag "socket" (optional string) ~doc:"PATH server socket path" in
     fun () -> Othrottle.Othrottle_client.add_job ~socket_path ~cmds ~origin)
;;

let kill_job_cmd =
  Command.async
    ~extract_exn:true
    ~summary:"Kill a currently running job"
    (let%map_open.Command cmd = anon ("job" %: string)
     and socket_path = flag "socket" (optional string) ~doc:"PATH server socket path" in
     fun () -> Othrottle.Othrottle_client.kill_job ~socket_path ~cmd)
;;

let server_cmd =
  Command.async
    ~extract_exn:true
    ~summary:"Run server daemon"
    (let%map_open.Command socket_path =
       flag "socket" (optional string) ~doc:"PATH server socket path"
     and config_path =
       flag "config" (optional string) ~doc:"PATH path to .toml configuration file"
     in
     fun () -> start_server socket_path config_path)
;;

let command =
  Command.group
    ~summary:"A job runner for asynchronous sequential commands"
    [ "server", server_cmd
    ; "status", status_cmd
    ; "run", job_cmd
    ; "kill", kill_job_cmd
    ; "config", config_cmd
    ]
;;

type build_info =
  { build_host : string
  ; build_user : string
  ; build_time : Time_float_unix.t
  ; commit_date : string
  ; ocaml_version : string
  }
[@@deriving sexp]

let build_info =
  { build_host = Bin_build_info.build_host
  ; build_user = Bin_build_info.build_user
  ; build_time = Time_float_unix.of_string Bin_build_info.build_time
  ; commit_date = Bin_build_info.commit_date
  ; ocaml_version = Bin_build_info.ocaml_version
  }
;;

let version =
  let git_hash = Bin_build_info.git_hash in
  let version = Bin_build_info.version in
  [%string "(https://git.sr.ht/~gary-kim/othrottle/commit/%{git_hash}) %{version}"]
;;

let () =
  Command_unix.run
    command
    ~version
    ~build_info:(sexp_of_build_info build_info |> Sexp.to_string_hum)
;;

open! Core
open! Async

let create_job_impl othrottle_state args =
  let cmds = Othrottle_protocol.Job.jobs args in
  let cmd, post_cmds =
    match cmds with
    | [] -> raise (Invalid_argument "missing any jobs")
    | f :: [] -> f, []
    | f :: p -> f, [ p ]
  in
  State.Othrottle_state.add_and_start_job
    othrottle_state
    ~cmd
    ~post_cmds
    ~origin:(Othrottle_protocol.Job.origin args)
;;

let kill_job_impl othrottle_state (args : Othrottle_protocol.Kill_job_query.t) =
  State.Othrottle_state.kill_job ~cmd:args.cmd othrottle_state
;;

let get_config_impl othrottle_state () = State.Othrottle_state.config othrottle_state

let status_impl othrottle_state (args : Othrottle_protocol.Status_query.t) =
  let filters =
    if args.include_finished
    then []
    else
      [ (fun a ->
          match State.Job_for_client.job_state a with
          | `Finished _ -> false
          | `Error _ | `Initialized | `Running _ | `Starting | `Terminated -> true)
      ]
  in
  let filters_merged =
    match List.reduce ~f:(fun l f v -> f v || l v) filters with
    | Some f -> f
    | None -> fun _ -> true
  in
  { Othrottle_protocol.Status.jobs =
      State.Othrottle_state.state othrottle_state |> List.filter ~f:filters_merged
  }
;;

let implementations : State.Othrottle_state.t Rpc.Implementation.t list =
  [ Rpc.Rpc.implement' Othrottle_protocol.create_job_rpc create_job_impl
  ; Rpc.Rpc.implement' Othrottle_protocol.status_rpc status_impl
  ; Rpc.Rpc.implement' Othrottle_protocol.kill_job_rpc kill_job_impl
  ; Rpc.Rpc.implement' Othrottle_protocol.get_config_rpc get_config_impl
  ]
;;

let start_rpc_server socket_path othrottle_state =
  let impls =
    Rpc.Implementations.create_exn
      ~implementations
      ~on_unknown_rpc:
        (`Call
          (fun _ ~rpc_tag ~version ->
            Log.Global.info "Unexpected RPC, tag %s, version %d" rpc_tag version;
            `Continue))
  in
  let where_to_listen = Tcp.Where_to_listen.of_file socket_path in
  let c =
    Rpc.Connection.serve_unix
      ~implementations:impls
      ~initial_connection_state:(fun _ _ _ -> othrottle_state)
      ~where_to_listen
      ()
  in
  Deferred.upon c (fun _ ->
    ignore (Unix.chmod socket_path ~perm:0o700 : unit Deferred.t);
    Signal.manage_by_async [ Signal.int; Signal.quit; Signal.term ];
    Shutdown.at_shutdown (fun () ->
      Log.Global.string "Removing socket before shutting down";
      Unix.unlink socket_path));
  c
;;

let get_config config_path_opt =
  match config_path_opt with
  | Some config_path ->
    (match Sys_unix.file_exists config_path with
     | `Unknown | `No ->
       Log.Global.error "cannot load config from the provided path";
       None
     | `Yes ->
       (match config_path |> Config.t_from_filepath with
        | Ok x -> Some x
        | Error x ->
          Log.Global.error "Cannot load config: %s" x;
          None))
  | None ->
    (match Config.get_config_path () |> Config.t_from_filepath with
     | Ok x -> Some x
     | Error x ->
       Log.Global.error "Cannot load config: %s" x;
       None)
;;

let get_socket_path socket_path_opt =
  let socket_path =
    match socket_path_opt with
    | Some x -> Some x
    | None -> Othrottle_socket.get_socket_path ()
  in
  match socket_path with
  | Some x -> Some x
  | None ->
    Log.Global.error "Cannot find suitable socket location";
    None
;;

let get_state config =
  match State.Othrottle_state.create ~config () with
  | Ok s -> Some s
  | Error e ->
    Log.Global.error "%s" @@ Error.to_string_hum e;
    None
;;

let init_and_recv socket_path ~config_path =
  (* GC on SIGUSR1 *)
  Signal.handle
    ~f:(fun _ ->
      Gc.compact ();
      Log.Global.info "Got SIGUSR1: Forcefully running GC")
    [ Signal.usr1 ];
  let%bind.Option config = get_config config_path in
  let%bind.Option othrottle_state = get_state config in
  let%bind.Option socket_path = get_socket_path socket_path in
  Option.return (start_rpc_server socket_path othrottle_state)
;;

open! Core
open! Async

module Job : sig
  type state =
    | Initialized
    | Starting
    | Terminated of { cleanup_evt : (string, unit) Clock_ns.Event.t }
    | Running of
        { start_time : Time_ns_unix.t
        ; proc : Process.t
        ; timeout_evt : (string, unit) Clock_ns.Event.t
        }
    | Timed_out of { restart_evt : (string, unit) Clock_ns.Event.t }
    | Error of
        { err : Error.t
        ; restart_evt_opt : (string, unit) Clock_ns.Event.t option
        }
    | Finished of
        { finish_time : Time_ns_unix.t
        ; cleanup_evt : (string, unit) Clock_ns.Event.t
        }

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_ns_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
end = struct
  type state =
    | Initialized
    | Starting
    | Terminated of { cleanup_evt : (string, unit) Clock_ns.Event.t }
    | Running of
        { start_time : Time_ns_unix.t
        ; proc : Process.t
        ; timeout_evt : (string, unit) Clock_ns.Event.t
        }
    | Timed_out of { restart_evt : (string, unit) Clock_ns.Event.t }
    | Error of
        { err : Error.t
        ; restart_evt_opt : (string, unit) Clock_ns.Event.t option
        }
    | Finished of
        { finish_time : Time_ns_unix.t
        ; cleanup_evt : (string, unit) Clock_ns.Event.t
        }

  type t =
    { (* cmd and name should basically always be the same. *)
      (* This may change later. *)
      name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_ns_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
end

module Job_for_client : sig
  type state =
    | Initialized
    | Starting
    | Terminated
    | Running of
        { start_time : Time_ns_unix.t
        ; pid : Pid.t
        }
    | Timed_out
    | Error of { err : Error.t }
    | Finished of { finish_time : Time_ns_unix.t }
  [@@deriving sexp, bin_io, compare]

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_ns_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
  [@@deriving sexp, bin_io, compare]

  val job_state : t -> state
  val t_of_job : Job.t -> t
  val to_markdown_table : t list -> string
end = struct
  type state =
    | Initialized
    | Starting
    | Terminated
    | Running of
        { start_time : Time_ns_unix.t
        ; pid : Pid.t
        }
    | Timed_out
    | Error of { err : Error.t }
    | Finished of { finish_time : Time_ns_unix.t }
  [@@deriving sexp, bin_io, compare]

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_ns_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
  [@@deriving sexp, bin_io, compare]

  let job_state j = j.job_state

  let t_of_job (j : Job.t) =
    let js =
      match j.job_state with
      | Initialized -> Initialized
      | Terminated _ -> Terminated
      | Running { start_time; proc; timeout_evt = _ } ->
        Running { start_time; pid = Process.pid proc }
      | Finished { finish_time; cleanup_evt = _ } -> Finished { finish_time }
      | Starting -> Starting
      | Timed_out { restart_evt = _ } -> Timed_out
      | Error { err; restart_evt_opt = _ } -> Error { err }
    in
    { name = j.name
    ; cmd = j.cmd
    ; post_cmds = j.post_cmds
    ; post_post_cmds = j.post_post_cmds
    ; last_queued = j.last_queued
    ; job_state = js
    ; origin = j.origin
    ; queued = j.queued
    ; retries = j.retries
    }
  ;;

  let uptime_seconds (j : t) =
    match j.job_state with
    | Running { start_time; _ } ->
      Time_ns_unix.diff (Time_ns_unix.now ()) start_time
      |> Time_ns_unix.Span.to_int_sec
      |> Int.to_string
    | Error _ | Finished _ | Starting | Initialized | Terminated | Timed_out -> ""
  ;;

  let to_markdown_table job_list =
    let str_pad str ?(padding = ' ') ?(right_pad = true) len =
      match right_pad with
      | true -> str ^ String.make (max 0 (len - String.length str)) padding
      | false -> String.make (max 0 (len - String.length str)) padding ^ str
    in
    let min_widths =
      [ ( "cmd"
        , ( List.fold_left job_list ~init:(String.length "cmd") ~f:(fun cm j ->
              max cm (String.length j.cmd))
            |> max (String.length "cmd")
          , true ) )
      ; ( "origin"
        , ( List.fold_left job_list ~init:(String.length "origin") ~f:(fun cm j ->
              max cm (String.length j.origin))
          , true ) )
      ; ( "queued"
        , ( List.fold_left job_list ~init:(String.length "queued") ~f:(fun cm j ->
              max cm (Int.to_string j.queued |> String.length))
          , false ) )
      ; ( "uptime (s)"
        , ( List.fold_left job_list ~init:(String.length "uptime (s)") ~f:(fun cm j ->
              max cm (uptime_seconds j |> String.length))
          , false ) )
      ]
    in
    let header =
      let cols =
        List.map min_widths ~f:(fun f -> str_pad (fst f) (snd f |> fst))
        |> String.concat ~sep:" | "
      in
      [%string "| %{cols} |"]
    in
    let separator =
      let cols =
        List.map min_widths ~f:(fun f ->
          let dashes = str_pad "" ~padding:'-' ((snd f |> fst) - 1) in
          if snd f |> snd then [%string ":%{dashes}"] else [%string "%{dashes}:"])
        |> String.concat ~sep:" | "
      in
      [%string "| %{cols} |"]
    in
    let contents =
      List.map job_list ~f:(fun j ->
        let cmd =
          str_pad j.cmd (List.Assoc.find_exn ~equal:String.equal min_widths "cmd" |> fst)
        in
        let origin =
          str_pad
            j.origin
            (List.Assoc.find_exn ~equal:String.equal min_widths "origin" |> fst)
        in
        let queued =
          str_pad
            (Int.to_string j.queued)
            ~right_pad:false
            (List.Assoc.find_exn ~equal:String.equal min_widths "queued" |> fst)
        in
        let uptime =
          str_pad
            (uptime_seconds j)
            ~right_pad:false
            (List.Assoc.find_exn ~equal:String.equal min_widths "uptime (s)" |> fst)
        in
        [%string "| %{cmd} | %{origin} | %{queued} | %{uptime} |"])
    in
    String.concat ~sep:"\n" ([ header; separator ] @ contents)
  ;;
end

module Othrottle_state : sig
  type t

  val create : config:Config.t -> unit -> t Or_error.t
  val state : t -> Job_for_client.t list
  val config : t -> Config.t
  val add_job : cmd:string -> post_cmds:string list list -> origin:string -> t -> unit
  val start_job : cmd:string -> t -> unit

  val add_and_start_job
    :  cmd:string
    -> post_cmds:string list list
    -> origin:string
    -> t
    -> unit

  val kill_job : cmd:string -> t -> unit Or_error.t
end = struct
  type t =
    { jobs : (string, Job.t) Hashtbl.t
    ; config : Config.t
    ; filters : (Re2.t * string) list
    ; event_pipe : State_event.t Pipe.Reader.t * State_event.t Pipe.Writer.t
    }

  let emit_event state event = Pipe.write_without_pushback (snd state.event_pipe) event

  let state othrottle_state =
    Hashtbl.to_alist othrottle_state.jobs
    |> List.map ~f:snd
    |> List.map ~f:Job_for_client.t_of_job
  ;;

  let config othrottle_state = othrottle_state.config

  let filtered_cmd ~cmd state =
    match state.filters |> List.find ~f:(fun r -> Re2.matches (fst r) cmd) with
    | None -> cmd
    | Some filter ->
      let pattern = Re2.to_string (fst filter) in
      [%log.info "matched, performing substitution" pattern];
      (* Should not fail since it has been prechecked when state was created *)
      Re2.rewrite_exn (fst filter) ~template:(snd filter) cmd
  ;;

  let append_if_non_empty f b =
    match b with
    | [ [] ] -> f
    | l -> List.append f l
  ;;

  let retry_timeout_for state (job : Job.t) =
    match state.config.retry_sequence.(job.retries) with
    | timeout -> timeout
    | exception _ ->
      (match Array.last_exn state.config.retry_sequence with
       | timeout -> timeout
       | exception _ -> 30)
  ;;

  let notify_failure_if_required ~(job : Job.t) state =
    match job.job_state with
    | Terminated _ | Running _ | Initialized | Starting | Finished _ -> ()
    | Timed_out _ | Error _ ->
      if job.retries + 1 = state.config.notify_on_counter
      then (
        let env = `Extend [ "JOB", job.cmd; "ORIGIN", job.origin ] in
        Deferred.upon
          (Process.run
             ~env
             ~prog:state.config.shell
             ~args:[ "-c"; state.config.notification_cmd ]
             ())
          (function
            | Ok _ -> ()
            | Error e ->
              let e = Error.tag e ~tag:"Got error running notify shell" in
              [%log.error (e : Error.t)]))
      else ()
  ;;

  let initialize_job ~cmd ~post_cmds ~origin =
    { Job.name = cmd
    ; cmd
    ; post_cmds
    ; post_post_cmds = []
    ; origin
    ; job_state = Initialized
    ; last_queued = Time_ns_unix.now ()
    ; queued = 0
    ; retries = 0
    }
  ;;

  let add_job ~cmd ~post_cmds ~origin state =
    let cmd = filtered_cmd ~cmd state in
    match Hashtbl.find state.jobs cmd with
    | Some j ->
      (match j.job_state with
       | Finished { finish_time = _; cleanup_evt = gc } | Terminated { cleanup_evt = gc }
         ->
         Clock_ns.Event.abort_if_possible gc cmd;
         Hashtbl.set state.jobs ~key:cmd ~data:(initialize_job ~cmd ~post_cmds ~origin)
       | Error { err = _; restart_evt_opt } ->
         (match restart_evt_opt with
          | Some evt -> Clock_ns.Event.abort_if_possible evt cmd
          | None -> ());
         Hashtbl.set state.jobs ~key:cmd ~data:(initialize_job ~cmd ~post_cmds ~origin)
       | Timed_out { restart_evt } ->
         Clock_ns.Event.abort_if_possible restart_evt cmd;
         Hashtbl.set state.jobs ~key:cmd ~data:(initialize_job ~cmd ~post_cmds ~origin)
       | Running _ | Starting ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { j with
               queued = j.queued + 1
             ; last_queued = Time_ns_unix.now ()
             ; post_post_cmds = append_if_non_empty j.post_post_cmds post_cmds
             }
       | Initialized ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { j with
               last_queued = Time_ns_unix.now ()
             ; post_post_cmds = append_if_non_empty j.post_post_cmds post_cmds
             })
    | None ->
      Hashtbl.add_exn state.jobs ~key:cmd ~data:(initialize_job ~cmd ~post_cmds ~origin)
  ;;

  let queue_cleanup ~cmd state =
    Clock_ns.Event.run_after
      (Time_ns.Span.of_int_sec state.config.job_timeout)
      (fun cmd -> emit_event state (State_event.JobCleanupRequested { cmd }))
      cmd
  ;;

  let monitor_job ~proc ~cmd state =
    (let%map po = Process.collect_output_and_wait proc in
     emit_event state (State_event.JobCompleted { cmd; exit_status = po.exit_status }))
    |> don't_wait_for
  ;;

  let start_job_async ~cmd state =
    (let%map proc_result =
       Process.create ~prog:state.config.shell ~args:[ "-c"; cmd ] ()
     in
     emit_event state (State_event.JobStartRequested { cmd; proc_result }))
    |> don't_wait_for
  ;;

  let handle_event state = function
    | State_event.JobRequested { cmd } ->
      let cmd = filtered_cmd ~cmd state in
      let pj = Hashtbl.find_exn state.jobs cmd in
      (match pj.job_state with
       | Initialized -> start_job_async ~cmd state
       | Starting | Running _ | Terminated _ | Timed_out _ | Error _ | Finished _ -> ())
    | State_event.JobStartRequested { cmd; proc_result } ->
      let j = Hashtbl.find_exn state.jobs cmd in
      (match proc_result with
       | Ok proc ->
         let timeout_evt =
           Clock_ns.Event.run_after
             (Time_ns_unix.Span.of_int_sec state.config.task_timeout)
             (fun cmd -> emit_event state (State_event.JobTimedOut { cmd }))
             cmd
         in
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { j with
               job_state = Running { start_time = Time_ns_unix.now (); proc; timeout_evt }
             };
         monitor_job ~proc ~cmd state
       | Error e ->
         [%log.info "Failed to start job" cmd];
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:{ j with job_state = Error { err = e; restart_evt_opt = None } })
    | State_event.JobCompleted { cmd; exit_status } ->
      let j = Hashtbl.find_exn state.jobs cmd in
      (match j.job_state with
       | Running { start_time = _; proc = _; timeout_evt } ->
         Clock_ns.Event.abort_if_possible timeout_evt cmd;
         (match Core_unix.Exit_or_signal.or_error exit_status with
          | Ok _ ->
            [%log.info "Finished job" (cmd : string)];
            Hashtbl.set
              state.jobs
              ~key:cmd
              ~data:
                { j with
                  job_state =
                    Finished
                      { finish_time = Time_ns_unix.now ()
                      ; cleanup_evt = queue_cleanup ~cmd state
                      }
                };
            List.map j.post_cmds ~f:(fun pc ->
              match pc with
              | [] -> ""
              | nc :: p ->
                add_job
                  ~cmd:nc
                  ~post_cmds:[ p ]
                  ~origin:[%string "Previous job: %{cmd}"]
                  state;
                nc)
            |> List.filter ~f:(fun a -> String.is_empty a |> not)
            |> List.iter ~f:(fun nc ->
              emit_event state (State_event.JobRequested { cmd = nc }));
            if j.queued > 0
            then (
              add_job
                ~cmd
                ~post_cmds:j.post_post_cmds
                ~origin:"Restarted from queued job"
                state;
              emit_event state (State_event.JobRequested { cmd }))
          | Error e ->
            [%log.info "Error-ed job" cmd];
            (match state.config.retry_on_error with
             | true ->
               [%log.info "retry_on_error enabled, setting task restart timer" cmd];
               let job =
                 { j with
                   job_state =
                     Error
                       { err = e
                       ; restart_evt_opt =
                           Some
                             (Clock_ns.Event.run_after
                                (retry_timeout_for state j |> Time_ns_unix.Span.of_int_sec)
                                (fun cmd ->
                                  emit_event
                                    state
                                    (State_event.JobRestartRequested { cmd }))
                                cmd)
                       }
                 }
               in
               Hashtbl.set state.jobs ~key:cmd ~data:job;
               notify_failure_if_required ~job state
             | false ->
               [%log.info
                 "retry_on_error disabled, setting error state for" (cmd : string)];
               let job =
                 { j with job_state = Error { err = e; restart_evt_opt = None } }
               in
               Hashtbl.set state.jobs ~key:cmd ~data:job;
               notify_failure_if_required ~job state))
       | Error _ | Terminated _ | Starting | Finished _ | Initialized | Timed_out _ -> ())
    | State_event.JobTimedOut { cmd } ->
      let pj = Hashtbl.find_exn state.jobs cmd in
      (match pj.job_state with
       | Running { start_time = _; proc; timeout_evt = _ } ->
         let data =
           { pj with
             job_state =
               Timed_out
                 { restart_evt =
                     Clock_ns.Event.run_after
                       (retry_timeout_for state pj |> Time_ns_unix.Span.of_int_sec)
                       (fun cmd ->
                         emit_event state (State_event.JobRestartRequested { cmd }))
                       cmd
                 }
           }
         in
         Hashtbl.set state.jobs ~key:cmd ~data;
         [%log.info "Timing out job" (cmd : string)];
         Process.send_signal proc Signal.term;
         notify_failure_if_required ~job:data state
       | Error _ | Finished _ | Timed_out _ | Terminated _ | Starting | Initialized -> ())
    | State_event.JobRestartRequested { cmd } ->
      let pj = Hashtbl.find_exn state.jobs cmd in
      (match pj.job_state with
       | Error { err = _; restart_evt_opt = _ } | Timed_out { restart_evt = _ } ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:{ pj with job_state = Initialized; retries = pj.retries + 1 };
         emit_event state (State_event.JobRequested { cmd })
       | Running _ | Finished _ | Terminated _ | Starting | Initialized -> ())
    | State_event.JobCleanupRequested { cmd } ->
      (match Hashtbl.find state.jobs cmd with
       | Some j ->
         (match j.job_state with
          | Finished _ | Terminated _ -> Hashtbl.remove state.jobs cmd
          | Error _ | Running _ | Initialized | Starting | Timed_out _ -> ())
       | None -> ())
    | State_event.JobKillRequested { cmd } ->
      (match Hashtbl.find state.jobs cmd with
       | Some j ->
         (match j.job_state with
          | Timed_out { restart_evt } ->
            Clock_ns.Event.abort_if_possible restart_evt cmd;
            Hashtbl.set
              state.jobs
              ~key:cmd
              ~data:
                { j with
                  job_state = Terminated { cleanup_evt = queue_cleanup ~cmd state }
                }
          | Error { err = _; restart_evt_opt } ->
            (match restart_evt_opt with
             | Some evt -> Clock_ns.Event.abort_if_possible evt cmd
             | None -> ());
            Hashtbl.set
              state.jobs
              ~key:cmd
              ~data:
                { j with
                  job_state = Terminated { cleanup_evt = queue_cleanup ~cmd state }
                }
          | Running { start_time = _; proc; timeout_evt } ->
            Clock_ns.Event.abort_if_possible timeout_evt cmd;
            Hashtbl.set
              state.jobs
              ~key:cmd
              ~data:
                { j with
                  job_state = Terminated { cleanup_evt = queue_cleanup ~cmd state }
                };
            Process.send_signal proc Signal.int
          | Starting | Initialized | Finished _ | Terminated _ -> ())
       | None -> ())
  ;;

  let rec process_events state =
    Pipe.read (fst state.event_pipe)
    >>= function
    | `Ok event ->
      handle_event state event;
      process_events state
    | `Eof -> return ()
  ;;

  let start_job ~cmd state =
    let cmd = filtered_cmd ~cmd state in
    let pj = Hashtbl.find_exn state.jobs cmd in
    match pj.job_state with
    | Initialized ->
      [%log.info "Starting job" (cmd : string)];
      Hashtbl.set state.jobs ~key:cmd ~data:{ pj with job_state = Starting };
      emit_event state (State_event.JobRequested { cmd })
    | Starting | Running _ | Terminated _ | Timed_out _ | Error _ | Finished _ -> ()
  ;;

  let add_and_start_job ~cmd ~post_cmds ~origin state =
    add_job ~cmd ~post_cmds ~origin state;
    start_job ~cmd state
  ;;

  let create ~config () =
    let x =
      { jobs = Hashtbl.create ~growth_allowed:true ~size:16 (module String)
      ; config
      ; filters =
          config.filters |> List.map ~f:(fun r -> Re2.of_string r.pattern, r.substitute)
      ; event_pipe = Pipe.create ()
      }
    in
    match
      List.filter x.filters ~f:(fun r ->
        not (Re2.valid_rewrite_template (fst r) ~template:(snd r)))
    with
    | [] ->
      process_events x |> don't_wait_for;
      Ok x
    | err ->
      let errors = err |> List.to_string ~f:(Fn.compose Re2.to_string fst) in
      Error
        (Error.create_s
           [%message "filter pattern(s) or their substitution(s) are invalid" errors])
  ;;

  let kill_job ~cmd state =
    match Hashtbl.mem state.jobs cmd with
    | false ->
      Error (Error.create_s [%message "Cannot find job with given cmd" (cmd : string)])
    | true -> Ok (emit_event state (State_event.JobKillRequested { cmd }))
  ;;
end

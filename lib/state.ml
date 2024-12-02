open! Core
open! Async

module Job : sig
  type state =
    [ `Initialized
    | `Starting
    | `Terminated of (string, unit) Clock.Event.t
    | `Running of Time_float_unix.t * Process.t * (string, unit) Clock.Event.t
    | `Timed_out of (string, unit) Clock.Event.t
    | `Error of Error.t * (string, unit) Clock.Event.t option
    | `Finished of Time_float_unix.t * (string, unit) Clock.Event.t
    ]

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_float_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
end = struct
  type state =
    [ `Initialized
    | `Starting
    | `Terminated of (string, unit) Clock.Event.t
    | `Running of Time_float_unix.t * Process.t * (string, unit) Clock.Event.t
    | `Timed_out of (string, unit) Clock.Event.t
    | `Error of Error.t * (string, unit) Clock.Event.t option
    | `Finished of Time_float_unix.t * (string, unit) Clock.Event.t
    ]

  type t =
    { (* cmd and name should basically always be the same. *)
      (* This may change later. *)
      name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_float_unix.t
    ; job_state : state
    ; origin : string
    ; queued : int
    ; retries : int
    }
end

module Job_for_client : sig
  type state =
    [ `Initialized
    | `Starting
    | `Terminated
    | `Running of Time_float_unix.t * Pid.t
    | `Timed_out
    | `Error of Error.t
    | `Finished of Time_float_unix.t
    ]
  [@@deriving sexp, bin_io, compare]

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_float_unix.t
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
    [ `Initialized
    | `Starting
    | `Terminated
    | `Running of Time_float_unix.t * Pid.t
    | `Timed_out
    | `Error of Error.t
    | `Finished of Time_float_unix.t
    ]
  [@@deriving sexp, bin_io, compare]

  type t =
    { name : string
    ; cmd : string
    ; post_cmds : string list list
    ; post_post_cmds : string list list
    ; last_queued : Time_float_unix.t
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
      | `Initialized -> `Initialized
      | `Terminated _ -> `Terminated
      | `Running (x, y, _) -> `Running (x, Process.pid y)
      | `Finished (x, _) -> `Finished x
      | `Starting -> `Starting
      | `Timed_out _ -> `Timed_out
      | `Error (x, _) -> `Error x
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
    | `Running (x, _) ->
      Time_float_unix.diff (Time_float_unix.now ()) x
      |> Time_float_unix.Span.to_sec
      |> Float.iround_down_exn
      |> Int.to_string
    | `Error _ | `Finished _ | `Starting | `Initialized | `Terminated | `Timed_out -> ""
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
      List.map min_widths ~f:(fun f -> str_pad (fst f) (snd f |> fst))
      |> String.concat ~sep:" | "
      |> Printf.sprintf "| %s |"
    in
    let seperator =
      List.map min_widths ~f:(fun f ->
        str_pad "" ~padding:'-' ((snd f |> fst) - 1)
        |> Printf.sprintf
             (match snd f |> snd with
              | true -> ":%s"
              | false -> "%s:"))
      |> String.concat ~sep:" | "
      |> Printf.sprintf "| %s |"
    in
    let contents =
      List.map job_list ~f:(fun j ->
        Printf.sprintf
          "| %s | %s | %s | %s |"
          (str_pad
             j.cmd
             (List.Assoc.find_exn ~equal:String.equal min_widths "cmd" |> fst))
          (str_pad
             j.origin
             (List.Assoc.find_exn ~equal:String.equal min_widths "origin" |> fst))
          (str_pad
             (Int.to_string j.queued)
             ~right_pad:false
             (List.Assoc.find_exn ~equal:String.equal min_widths "queued" |> fst))
          (str_pad
             (uptime_seconds j)
             ~right_pad:false
             (List.Assoc.find_exn ~equal:String.equal min_widths "uptime (s)" |> fst)))
    in
    String.concat ~sep:"\n" ([ header; seperator ] @ contents)
  ;;
end

module Othrottle_state : sig
  type t

  val create : config:Config.t -> unit -> (t, Error.t) Result.t
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

  val kill_job : cmd:string -> t -> (unit, Error.t) result
end = struct
  type t =
    { jobs : (string, Job.t) Hashtbl.t
    ; config : Config.t
    ; filters : (Re2.t * string) list
    ; job_pipe : string Pipe.Reader.t * string Pipe.Writer.t
    }

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
      Log.Global.info "matched '%s', performing substitution" (Re2.to_string (fst filter));
      (* Should not fail since it has been prechecked when state was created *)
      Re2.rewrite_exn (fst filter) ~template:(snd filter) cmd
  ;;

  let append_if_non_empty f b =
    match b with
    | [ [] ] -> f
    | l -> List.append f l
  ;;

  let retry_timeout_for state (job : Job.t) =
    match Array.length state.config.retry_sequence < job.retries with
    | true ->
      (match Array.length state.config.retry_sequence with
       | 0 -> 30 (* Default to 30 seconds retry timeout *)
       | _ -> Array.last state.config.retry_sequence)
    | false -> Array.unsafe_get state.config.retry_sequence job.retries
  ;;

  let queue_start_job ~cmd state = don't_wait_for @@ Pipe.write (snd state.job_pipe) cmd

  let add_job ~cmd ~post_cmds ~origin state =
    let cmd = filtered_cmd ~cmd state in
    match Hashtbl.find state.jobs cmd with
    | Some j ->
      (match j.job_state with
       | `Finished (_, gc) | `Terminated gc ->
         Clock.Event.abort_if_possible gc cmd;
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { name = cmd
             ; cmd
             ; post_cmds
             ; post_post_cmds = []
             ; origin
             ; job_state = `Initialized
             ; last_queued = Time_float_unix.now ()
             ; queued = 0
             ; retries = 0
             }
       | `Error (_, evt_opt) ->
         (match evt_opt with
          | Some evt -> Clock.Event.abort_if_possible evt cmd
          | None -> ());
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { name = cmd
             ; cmd
             ; post_cmds
             ; post_post_cmds = []
             ; origin
             ; job_state = `Initialized
             ; last_queued = Time_float_unix.now ()
             ; queued = 0
             ; retries = 0
             }
       | `Timed_out evt ->
         Clock.Event.abort_if_possible evt cmd;
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { name = cmd
             ; cmd
             ; post_cmds
             ; post_post_cmds = []
             ; origin
             ; job_state = `Initialized
             ; last_queued = Time_float_unix.now ()
             ; queued = 0
             ; retries = 0
             }
       | `Running _ | `Starting ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { j with
               queued = j.queued + 1
             ; last_queued = Time_float_unix.now ()
             ; post_post_cmds = append_if_non_empty j.post_post_cmds post_cmds
             }
       | `Initialized ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:
             { j with
               last_queued = Time_float_unix.now ()
             ; post_post_cmds = append_if_non_empty j.post_post_cmds post_cmds
             })
    | None ->
      Hashtbl.add_exn
        state.jobs
        ~key:cmd
        ~data:
          { name = cmd
          ; cmd
          ; post_cmds
          ; post_post_cmds = []
          ; origin
          ; job_state = `Initialized
          ; last_queued = Time_float_unix.now ()
          ; queued = 0
          ; retries = 0
          }
  ;;

  let job_cleanup ~cmd state =
    match Hashtbl.find state.jobs cmd with
    | Some j ->
      (match j.job_state with
       | `Finished _ | `Terminated _ -> Hashtbl.remove state.jobs cmd
       | `Error _ | `Running _ | `Initialized | `Starting | `Timed_out _ -> ())
    | None -> ()
  ;;

  let queue_cleanup ~cmd state =
    Clock.Event.run_after
      (Time_float.Span.of_int_sec state.config.job_timeout)
      (fun cmd -> job_cleanup ~cmd state)
      cmd
  ;;

  let finished_enum ~cmd state = Time_float_unix.now (), queue_cleanup ~cmd state

  let rec monitor_job ~proc ~cmd state =
    let%bind po = Process.collect_output_and_wait proc in
    return
      (let eos = po.exit_status in
       let j = Hashtbl.find_exn state.jobs cmd in
       match j.job_state with
       | `Error _ | `Terminated _ | `Starting | `Finished _ | `Initialized | `Timed_out _
         -> ()
       | `Running (_, _, timeout_evt) ->
         Clock.Event.abort_if_possible timeout_evt cmd;
         (match Core_unix.Exit_or_signal.or_error eos with
          | Ok _ ->
            Log.Global.printf "Finished job: \"%s\"" cmd;
            Hashtbl.set
              state.jobs
              ~key:cmd
              ~data:{ j with job_state = `Finished (finished_enum ~cmd state) };
            (* Start post_cmds job *)
            List.map j.post_cmds ~f:(fun pc ->
              match pc with
              | [] -> ""
              | nc :: p ->
                add_job
                  ~cmd:nc
                  ~post_cmds:[ p ]
                  ~origin:(Printf.sprintf "Previous job: %s" cmd)
                  state;
                nc)
            |> List.filter ~f:(fun a -> String.is_empty a |> not)
            |> List.iter ~f:(fun nc -> start_job ~cmd:nc state);
            (* Start successive job if jobs are queued *)
            if j.queued > 0
            then
              add_and_start_job
                ~cmd
                ~post_cmds:j.post_post_cmds
                ~origin:"Restarted from queued job"
                state
          | Error e ->
            Log.Global.printf "Error-ed job: \"%s\"" cmd;
            (match state.config.retry_on_error with
             | true ->
               Log.Global.printf
                 "try_on_error enabled, setting task restart timer for \"%s\""
                 cmd;
               Hashtbl.set
                 state.jobs
                 ~key:cmd
                 ~data:
                   { j with
                     job_state =
                       `Error
                         ( e
                         , Some
                             (Clock.Event.run_after
                                (retry_timeout_for state j
                                 |> Time_float_unix.Span.of_int_sec)
                                (fun cmd -> task_restart ~cmd state)
                                cmd) )
                   }
             | false ->
               Log.Global.printf
                 "retry_on_error disabled, setting error state for \"%s\""
                 cmd;
               Hashtbl.set
                 state.jobs
                 ~key:cmd
                 ~data:{ j with job_state = `Error (e, None) })))

  and task_timeout ~cmd state =
    let pj = Hashtbl.find_exn state.jobs cmd in
    match pj.job_state with
    | `Error _
    | `Finished (_, _)
    | `Timed_out _ | `Terminated _ | `Starting | `Initialized -> ()
    | `Running (_, proc, _) ->
      Hashtbl.set
        state.jobs
        ~key:cmd
        ~data:
          { pj with
            job_state =
              `Timed_out
                (Clock.Event.run_after
                   (retry_timeout_for state pj |> Time_float_unix.Span.of_int_sec)
                   (fun cmd -> task_restart ~cmd state)
                   cmd)
          };
      Log.Global.printf "Timing out job: \"%s\"" cmd;
      Process.send_signal proc Signal.term

  and task_restart ~cmd state =
    let pj = Hashtbl.find_exn state.jobs cmd in
    match pj.job_state with
    | `Running (_, _, _) | `Finished (_, _) | `Terminated _ | `Starting | `Initialized ->
      ()
    | `Error (_, _) | `Timed_out _ ->
      Hashtbl.set
        state.jobs
        ~key:cmd
        ~data:{ pj with job_state = `Initialized; retries = pj.retries + 1 };
      start_job ~cmd state

  and start_job ~cmd state =
    let cmd = filtered_cmd ~cmd state in
    let pj = Hashtbl.find_exn state.jobs cmd in
    match pj.job_state with
    | `Initialized ->
      Log.Global.printf "Starting job: \"%s\"" cmd;
      Hashtbl.set state.jobs ~key:cmd ~data:{ pj with job_state = `Starting };
      don't_wait_for
      @@
      let%bind poe = Process.create ~prog:state.config.shell ~args:[ "-c"; cmd ] () in
      return
        (let j = Hashtbl.find_exn state.jobs cmd in
         let timeout_evt =
           Clock.Event.run_after
             (Time_float_unix.Span.of_int_sec state.config.task_timeout)
             (fun cmd -> task_timeout ~cmd state)
             cmd
         in
         match poe with
         | Ok proc ->
           Hashtbl.set
             state.jobs
             ~key:cmd
             ~data:
               { j with job_state = `Running (Time_float_unix.now (), proc, timeout_evt) };
           don't_wait_for @@ monitor_job ~proc ~cmd state
         | Error e ->
           Log.Global.printf "Failed to start job \"%s\"" cmd;
           Hashtbl.set state.jobs ~key:cmd ~data:{ j with job_state = `Error (e, None) })
    | _ -> ()

  and add_and_start_job ~cmd ~post_cmds ~origin state =
    add_job ~cmd ~post_cmds ~origin state;
    queue_start_job ~cmd state
  ;;

  let rec process_pipe state =
    Pipe.read (fst state.job_pipe)
    >>= function
    | `Ok cmd ->
      start_job ~cmd state;
      process_pipe state
    | `Eof -> return ()
  ;;

  let create ~config () =
    let x =
      { jobs = Hashtbl.create ~growth_allowed:true ~size:16 (module String)
      ; config
      ; filters =
          config.filters |> List.map ~f:(fun r -> Re2.of_string r.pattern, r.substitute)
      ; job_pipe = Pipe.create ()
      }
    in
    match
      List.filter x.filters ~f:(fun r ->
        not (Re2.valid_rewrite_template (fst r) ~template:(snd r)))
    with
    | [] ->
      don't_wait_for @@ process_pipe x;
      Ok x
    | err ->
      Result.Error
        (Error.of_string
           (Printf.sprintf
              "filter pattern(s) '%s' or their substitutions are invalid"
              (List.to_string ~f:(fun s -> Re2.to_string (fst s)) err)))
  ;;

  let kill_job ~cmd state =
    match Hashtbl.find state.jobs cmd with
    | None -> Error (Error.createf "Cannot find job with ~cmd:\"%s\"" cmd)
    | Some j ->
      (match j.job_state with
       | `Error _ | `Starting | `Initialized | `Finished _ | `Terminated _ | `Timed_out _
         -> Ok ()
       | `Running (_, proc, _) ->
         Hashtbl.set
           state.jobs
           ~key:cmd
           ~data:{ j with job_state = `Terminated (queue_cleanup ~cmd state) };
         Process.send_signal proc Signal.int;
         Ok ())
  ;;
end

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

  type t
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
end

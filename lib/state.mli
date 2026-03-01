open! Core
open! Async

module Job : sig
  type state
  type t
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
end

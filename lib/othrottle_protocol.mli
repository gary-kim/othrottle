open! Core
open! Async

module Job : sig
  type t =
    { jobs : string list
    ; origin : string
    }
  [@@deriving sexp, bin_io, compare]

  val jobs : t -> string list
  val origin : t -> string
end

module Status_query : sig
  type t = { include_finished : bool } [@@deriving sexp, bin_io, compare]
end

module Status : sig
  type t = { jobs : State.Job_for_client.t list } [@@deriving sexp, bin_io, compare]

  val create : jobs:State.Job_for_client.t list -> t
  val to_markdown_table : t -> string
end

module Kill_job_query : sig
  type t = { cmd : string } [@@deriving sexp, bin_io, compare]
end

val create_job_rpc : (Job.t, unit) Rpc.Rpc.t
val kill_job_rpc : (Kill_job_query.t, unit Or_error.t) Rpc.Rpc.t
val status_rpc : (Status_query.t, Status.t) Rpc.Rpc.t
val get_config_rpc : (unit, Config.t) Rpc.Rpc.t

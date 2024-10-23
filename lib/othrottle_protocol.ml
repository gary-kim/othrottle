open! Core
open! Async

module Job = struct
  type t =
    { jobs : string list
    ; origin : string
    }
  [@@deriving sexp, bin_io, compare]

  let jobs job = job.jobs
  let origin job = job.origin
end

module Status_query = struct
  type t = { include_finished : bool } [@@deriving sexp, bin_io, compare]
end

module Status = struct
  type t = { jobs : State.Job_for_client.t list } [@@deriving sexp, bin_io, compare]

  let create ~jobs = { jobs }
  let to_markdown_table s = State.Job_for_client.to_markdown_table s.jobs
end

module Kill_job_query = struct
  type t = { cmd : string } [@@deriving sexp, bin_io, compare]
end

let create_job_rpc =
  Rpc.Rpc.create
    ~name:"create_job"
    ~version:0
    ~bin_query:[%bin_type_class: Job.t]
    ~bin_response:[%bin_type_class: unit]
    ~include_in_error_count:Only_on_exn
;;

let kill_job_rpc =
  Rpc.Rpc.create
    ~name:"kill_job"
    ~version:0
    ~bin_query:[%bin_type_class: Kill_job_query.t]
    ~bin_response:[%bin_type_class: (unit, Error.t) result]
    ~include_in_error_count:Only_on_exn
;;

let status_rpc =
  Rpc.Rpc.create
    ~name:"status"
    ~version:0
    ~bin_query:[%bin_type_class: Status_query.t]
    ~bin_response:[%bin_type_class: Status.t]
    ~include_in_error_count:Only_on_exn
;;

let get_config_rpc =
  Rpc.Rpc.create
    ~name:"get_config"
    ~version:0
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: Config.t]
    ~include_in_error_count:Only_on_exn
;;

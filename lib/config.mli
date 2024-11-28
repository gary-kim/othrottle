open! Core
open! Async

type filter =
  { pattern : string
  ; substitute : string
  }
[@@deriving sexp, bin_io, compare]

type t =
  { shell : string
  ; job_timeout : int
  ; task_timeout : int
  ; retry_sequence : int array
  ; retry_on_error : bool
  ; notification_cmd : string
  ; filters : filter list
  }
[@@deriving sexp, bin_io, compare]

val otoml_of_t : t -> Otoml.t
val get_config_path : unit -> string
val t_from_filepath : string -> (t, string) Result.t

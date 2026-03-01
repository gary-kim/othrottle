open! Core
open! Async

module Status_output_format : sig
  type t =
    | Sexp
    | Markdown
  [@@deriving enumerate, sexp, string]

  val arg_type : t Command.Arg_type.t
end

module Get_config_output_format : sig
  type t =
    | Sexp
    | Toml
  [@@deriving enumerate, sexp, string]

  val arg_type : t Command.Arg_type.t
end

val add_job
  :  socket_path:string option
  -> cmds:string list
  -> origin:string
  -> unit Deferred.t

val status
  :  socket_path:string option
  -> include_finished:bool
  -> Status_output_format.t
  -> unit Deferred.t

val kill_job : socket_path:string option -> cmd:string -> unit Deferred.t

val get_config
  :  socket_path:string option
  -> Get_config_output_format.t
  -> unit Deferred.t

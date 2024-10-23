open! Base
open! Async

val is_possible_path : string -> bool
val get_possible_socket_paths : unit -> string list
val get_socket_path : unit -> string option

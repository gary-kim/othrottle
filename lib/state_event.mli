open! Core
open Async

(** Loggable event type that can be serialized and persisted *)
module Loggable : sig
  type t =
    | JobRequested of { cmd : string }
    | JobStartRequested of { cmd : string }
    | JobStartSucceeded of { cmd : string }
    | JobStartFailed of
        { cmd : string
        ; error : Error.t
        }
    | JobCompleted of
        { cmd : string
        ; exit_code : int
        }
    | JobTimedOut of { cmd : string }
    | JobRestartRequested of { cmd : string }
    | JobCleanupRequested of { cmd : string }
    | JobKillRequested of { cmd : string }
  [@@deriving sexp, bin_io, compare]
end

(** Internal event type used for event-driven state transitions *)
type t =
  | JobRequested of { cmd : string }
  | JobStartRequested of
      { cmd : string
      ; proc_result : Process.t Or_error.t
      }
  | JobCompleted of
      { cmd : string
      ; exit_status : Core_unix.Exit_or_signal.t
      }
  | JobTimedOut of { cmd : string }
  | JobRestartRequested of { cmd : string }
  | JobCleanupRequested of { cmd : string }
  | JobKillRequested of { cmd : string }

(** Convert an internal event to its loggable representation *)
val to_loggable : t -> Loggable.t

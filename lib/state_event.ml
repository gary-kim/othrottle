open! Core
open Async

module Loggable = struct
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

let to_loggable : t -> Loggable.t = function
  | JobRequested { cmd } -> Loggable.JobRequested { cmd }
  | JobStartRequested { cmd; proc_result } ->
    (match proc_result with
     | Ok _ -> Loggable.JobStartSucceeded { cmd }
     | Error error -> Loggable.JobStartFailed { cmd; error })
  | JobCompleted { cmd; exit_status } ->
    let exit_code =
      match exit_status with
      | Ok () -> 0
      | Error (`Exit_non_zero i) -> i
      | Error (`Signal s) -> Signal_unix.to_system_int s
    in
    Loggable.JobCompleted { cmd; exit_code }
  | JobTimedOut { cmd } -> Loggable.JobTimedOut { cmd }
  | JobRestartRequested { cmd } -> Loggable.JobRestartRequested { cmd }
  | JobCleanupRequested { cmd } -> Loggable.JobCleanupRequested { cmd }
  | JobKillRequested { cmd } -> Loggable.JobKillRequested { cmd }
;;

open! Core
open! Async

let%expect_test "config with existing config" =
  (match Othrottle.Config.t_from_filepath "samples/config.toml" with
   | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string_hum |> print_endline
   | Error error -> print_s [%sexp (error : Error.t)]);
  [%expect
    {|
    ((shell bash) (job_timeout 30) (task_timeout 1000) (retry_sequence (5 10 30))
     (retry_on_error true) (notification_cmd "") (notify_on_counter 2)
     (filters ()))
    |}];
  return ()
;;

let%expect_test "config with all default values" =
  (match Othrottle.Config.t_from_filepath "./config.toml" with
   | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string_hum |> print_endline
   | Error error -> print_s [%sexp (error : Error.t)]);
  [%expect
    {|
    ((shell bash) (job_timeout 600) (task_timeout 30)
     (retry_sequence (5 15 30 60 120 300 900)) (retry_on_error true)
     (notification_cmd "") (notify_on_counter 2) (filters ()))
    |}];
  return ()
;;

let%expect_test "config with invalid toml" =
  (match Othrottle.Config.t_from_filepath "samples/invalid_config.toml" with
   | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string_hum |> print_endline
   | Error error -> print_s [%sexp (error : Error.t)]);
  [%expect
    {|
    ("otoml parse error" (pos ((1 14)))
     (err "Malformed key-value pair (missing value?)"))
    |}];
  return ()
;;

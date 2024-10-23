open! Core
open Stdio

(* Config with existing config *)
let () =
  let () =
    match Othrottle.Config.t_from_filepath "../../../test/samples/config.toml" with
    | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string |> print_endline
    | Error err -> print_endline err
  in
  ()
;;

(* Config with all default values *)
let () =
  let () =
    match Othrottle.Config.t_from_filepath "./config.toml" with
    | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string |> print_endline
    | Error err -> print_endline err
  in
  ()
;;

(* Config with invalid toml *)
let () =
  let () =
    match
      Othrottle.Config.t_from_filepath "../../../test/samples/invalid_config.toml"
    with
    | Ok config -> Othrottle.Config.sexp_of_t config |> Sexp.to_string |> print_endline
    | Error err -> print_endline err
  in
  ()
;;

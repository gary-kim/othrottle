open! Core
open! Async

let is_possible_path possible_path =
  let base_path = Filename.dirname possible_path in
  match Sys_unix.is_directory base_path with
  | `Yes -> true
  | `Unknown | `No -> false
;;

let get_possible_socket_paths () =
  let ( / ) = Filename.concat in
  let xdg = Xdg.create ~env:Sys.getenv () in
  let xdg_path =
    match Xdg.runtime_dir xdg with
    | Some runtime_dir -> [ runtime_dir / "othrottle.sock" ]
    | None -> []
  in
  let uid = Core_unix.getuid () in
  let tmp_path = [ "/tmp/" / Printf.sprintf "%d-othrottle.sock" uid ] in
  xdg_path @ tmp_path
;;

let get_socket_path () = get_possible_socket_paths () |> List.find ~f:is_possible_path

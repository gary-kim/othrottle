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
  ; notification_cmd : string
  ; filters : filter list
  }
[@@deriving sexp, bin_io, compare]

let get_config_path () =
  let xdg = Xdg.create ~env:Sys.getenv () in
  let ( / ) = Filename.concat in
  Xdg.config_dir xdg / "othrottle" / "config.toml"
;;

let otoml_of_t c =
  let update k v o = Otoml.update o [ k ] (Some v) in
  Otoml.Parser.from_string ""
  |> update "job_timeout" (Otoml.integer c.job_timeout)
  |> update "task_timeout" (Otoml.integer c.task_timeout)
  |> update "shell" (Otoml.string c.shell)
  |> update "notification_cmd" (Otoml.string c.notification_cmd)
  |> update
       "retry_sequence"
       (Otoml.array
          (c.retry_sequence |> List.of_array |> List.map ~f:(fun x -> Otoml.integer x)))
  |> update
       "filters"
       (Otoml.array
          (c.filters
           |> List.map ~f:(fun x ->
             Otoml.table
               [ "pattern", Otoml.string x.pattern
               ; "substitute", Otoml.string x.substitute
               ])))
;;

let t_from_filepath filepath =
  let c =
    try Ok (Otoml.Parser.from_file filepath) with
    | Sys_error _ -> Ok (Otoml.string "")
    | Otoml.Parse_error (pos, err) -> Error (Otoml.Parser.format_parse_error pos err)
    | Failure err -> Error (Printf.sprintf "otoml internal error: %s" err)
  in
  match c with
  | Ok conf ->
    Result.map_error ~f:(fun e -> Exn.to_string e)
    @@ Result.try_with (fun () ->
      { job_timeout = Otoml.find_or ~default:600 conf Otoml.get_integer [ "job_timeout" ]
      ; task_timeout = Otoml.find_or ~default:30 conf Otoml.get_integer [ "task_timeout" ]
      ; shell = Otoml.find_or ~default:"bash" conf Otoml.get_string [ "shell" ]
      ; notification_cmd =
          Otoml.find_or ~default:"" conf Otoml.get_string [ "notification_cmd" ]
      ; retry_sequence =
          Otoml.find_or
            ~default:[ 5; 15; 30; 60; 120; 300; 900 ]
            conf
            (Otoml.get_array Otoml.get_integer)
            [ "retry_sequence" ]
          |> Array.of_list
      ; filters =
          (match Otoml.path_exists conf [ "filters" ] with
           | true ->
             Otoml.find_exn
               conf
               (Otoml.get_array ~strict:true (fun o ->
                  { pattern = Otoml.find_exn o Otoml.get_string [ "pattern" ]
                  ; substitute = Otoml.find_exn o Otoml.get_string [ "substitute" ]
                  }))
               [ "filters" ]
           | false -> [])
      })
  | Error err -> Error err
;;

let%expect_test "test default config through t_from_filepath" =
  let c = t_from_filepath "/dev/null" |> Result.ok_or_failwith in
  otoml_of_t c |> Otoml.Printer.to_string |> print_string;
  return
    [%expect
      {|
      job_timeout = 600
      task_timeout = 30
      shell = "bash"
      notification_cmd = ""
      retry_sequence = [5, 15, 30, 60, 120, 300, 900]
      filters = []
      |}]
;;

let%expect_test "test otoml_of_t" =
  let c =
    { shell = "bash"
    ; job_timeout = 30
    ; task_timeout = 30
    ; retry_sequence = [| 5 |]
    ; notification_cmd = ""
    ; filters = []
    }
  in
  otoml_of_t c |> Otoml.Printer.to_string |> print_string;
  return
    [%expect
      {|
      job_timeout = 30
      task_timeout = 30
      shell = "bash"
      notification_cmd = ""
      retry_sequence = [5]
      filters = []
      |}]
;;

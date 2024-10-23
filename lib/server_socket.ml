open! Core
open! Async

module Listening_socket : sig
  type t = ([ `Passive ], Socket.Address.Unix.t) Socket.t [@@deriving sexp_of]

  val create : string -> t Deferred.t
end = struct
  type t = ([ `Passive ], Socket.Address.Unix.t) Socket.t [@@deriving sexp_of]

  let create socket_path =
    let socket = Socket.create Unix.Socket.Type.unix in
    let sockaddr = Socket.Address.Unix.create socket_path in
    let%bind bound_socket = Socket.bind socket sockaddr in
    let%bind _ = Unix.chmod socket_path ~perm:0o700 in
    return
      (Signal.manage_by_async [ Signal.int; Signal.quit; Signal.term ];
       Shutdown.at_shutdown (fun () ->
         Log.Global.string "Removing socket before shutting down";
         Unix.unlink socket_path);
       Socket.listen bound_socket ~backlog:16)
  ;;
end

module Client_connection : sig
  type t [@@deriving sexp_of]

  val create : Listening_socket.t -> t option Deferred.t
  val next_message : ?buffer_size:int -> t -> Bytes.t option Deferred.t
  val send_message : t -> string -> unit
  val rpc_transport_of_t : t -> Rpc.Transport.t
  val reader : t -> Reader.t
  val writer : t -> Writer.t
  val close : t -> unit
end = struct
  type t =
    { socket : ([ `Active ], Socket.Address.Unix.t) Socket.t
    ; writer : Writer.t
    ; reader : Reader.t
    }
  [@@deriving sexp_of]

  let create socket =
    let%bind socket_result = Socket.accept socket in
    return
      (match socket_result with
       | `Ok socket_res ->
         let client_socket, _ = socket_res in
         Some
           { socket = client_socket
           ; writer = Socket.fd client_socket |> Writer.create
           ; reader = Socket.fd client_socket |> Reader.create
           }
       | `Socket_closed -> None)
  ;;

  let next_message ?(buffer_size = 1024) client_conn =
    let buffer = Bytes.create buffer_size in
    let%bind read_result = Reader.read client_conn.reader buffer in
    return
      (match read_result with
       | `Ok bytes_read -> Some (Bytes.sub buffer ~pos:0 ~len:bytes_read)
       | `Eof -> None)
  ;;

  let send_message client_conn msg = Writer.write client_conn.writer msg
  let rpc_transport_of_t conn = Rpc.Transport.of_reader_writer conn.reader conn.writer
  let reader conn = conn.reader
  let writer conn = conn.writer
  let close client_conn = Socket.shutdown client_conn.socket `Both
end

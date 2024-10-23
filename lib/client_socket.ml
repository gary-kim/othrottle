open! Core
open! Async

module Connection : sig
  type t

  val create : string -> (t, Exn.t) Result.t Deferred.t
  val reader : t -> Reader.t
  val writer : t -> Writer.t
  val shutdown : t -> unit Deferred.t
end = struct
  type t =
    { socket : ([ `Active ], Socket.Address.Unix.t) Socket.t
    ; writer : Writer.t
    ; reader : Reader.t
    }

  let create socket_path =
    let sockaddr = Socket.Address.Unix.create socket_path in
    let socket = Socket.create Unix.Socket.Type.unix in
    try_with (fun () ->
      let%bind connected_socket = Socket.connect socket sockaddr in
      return
        { socket = connected_socket
        ; writer = Socket.fd connected_socket |> Writer.create
        ; reader = Socket.fd connected_socket |> Reader.create
        })
  ;;

  let reader conn = conn.reader
  let writer conn = conn.writer

  let shutdown s =
    let d = Deferred.all_unit [ Writer.close s.writer; Reader.close s.reader ] in
    Socket.shutdown s.socket `Both;
    d
  ;;
end

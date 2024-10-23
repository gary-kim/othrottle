open! Core
open! Async

module Listening_socket : sig
  type t [@@deriving sexp_of]

  val create : string -> t Async.Deferred.t
end

module Client_connection : sig
  type t [@@deriving sexp_of]

  val create : Listening_socket.t -> t option Async.Deferred.t
  val next_message : ?buffer_size:int -> t -> Core.Bytes.t option Async.Deferred.t
  val send_message : t -> string -> unit
  val rpc_transport_of_t : t -> Rpc.Transport.t
  val reader : t -> Reader.t
  val writer : t -> Writer.t
  val close : t -> unit
end

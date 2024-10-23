open! Core
open! Async

module Connection : sig
  type t

  val create : string -> (t, Exn.t) Result.t Deferred.t
  val reader : t -> Reader.t
  val writer : t -> Writer.t
  val shutdown : t -> unit Deferred.t
end

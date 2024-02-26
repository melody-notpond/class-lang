module type MONAD_INSTANCE = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type MONAD = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val (>>) : 'a t -> 'b t -> 'b t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  val foldM : ('b -> 'a -> 'b t) -> 'b -> 'a list -> 'b t
end

module Monad : functor (I: MONAD_INSTANCE) -> MONAD with type 'a t = 'a I.t

module OptionMonad : MONAD with type 'a t = 'a option

module ReaderMonad : functor (B: sig type t end) -> MONAD with type 'a t = B.t -> 'a

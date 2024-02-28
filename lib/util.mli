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

module Option : sig
  module Monad : MONAD with type 'a t = 'a option

  val error : 'a -> 'b Monad.t
end


module Result :
  functor (B: sig type t end) -> sig
    module Monad : MONAD with type 'a t = ('a, B.t) result

    val error : B.t -> 'a Monad.t
  end

module Reader :
  functor (B: sig type t end) -> sig
    module Monad : MONAD with type 'a t = B.t -> 'a

    val ask : B.t Monad.t
    val local : B.t Monad.t -> 'a Monad.t -> 'a Monad.t
  end

module ReaderResult :
  functor (B: sig type t type e end) -> sig
    module Monad : MONAD with type 'a t = B.t -> ('a, B.e) result

    val error : B.e -> 'a Monad.t
    val ask : B.t Monad.t
    val local : B.t Monad.t -> 'a Monad.t -> 'a Monad.t
  end

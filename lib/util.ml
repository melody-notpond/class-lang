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

module Monad : functor (I: MONAD_INSTANCE) -> MONAD with type 'a t = 'a I.t =
  functor (Inst: MONAD_INSTANCE) -> struct
    type 'a t = 'a Inst.t
    let (>>=) x f = Inst.(x >>= f)
    let return x = Inst.return x

    let (>>) a b = a >>= (fun _ -> b)
    let (let*) x f = x >>= f

    let rec foldM f a xs =
      match xs with
      | x :: xs ->
        let* v = f a x in
          foldM f v xs
      | [] -> return a
  end

module OptionMonadInst = struct
  type 'a t = 'a option

  let (>>=) x f =
    match x with
    | Some v -> f v
    | None -> None

  let return x = Some x
end

module Option = struct
  module Monad = Monad(OptionMonadInst)

  let error _ : 'a Monad.t = None
end

module ResultMonadInst =
  functor (B: sig type t end) -> struct
    type 'a t = ('a, B.t) result

    let (>>=) x f =
      match x with
      | Ok v -> f v
      | Error s -> Error s

    let return x = Ok x
  end

module Result =
  functor (B: sig type t end) -> struct
    module Monad = Monad(ResultMonadInst(B))

    let error e : 'a Monad.t = Error e
  end

module ReaderMonadInst =
  functor (B: sig type t end) -> struct
    type b = B.t
    type 'a t = b -> 'a

    let (>>=) f g = fun x -> g (f x) x
    let return x = fun _ -> x
  end

module Reader =
  functor (B: sig type t end) -> struct
    module Monad = Monad(ReaderMonadInst(B))

    let ask : B.t Monad.t = fun e -> e

    let local f m : 'a Monad.t =
      fun e -> m (f e)
  end

module ReaderResultMonadInst =
  functor (B: sig type t type e end) -> struct
    type 'a t = B.t -> ('a, B.e) result

    let (>>=) f g =
      fun x ->
      match f x with
      | Ok v -> g v x
      | Error s -> Error s

    let return x = fun _ -> Ok x
  end

module ReaderResult =
  functor (B: sig type t type e end) -> struct
    module Monad = Monad(ReaderResultMonadInst(B))

    let error s : 'a Monad.t =
      fun _ -> Error s

    let ask : B.t Monad.t = fun e -> Ok e

    let local f m : 'a Monad.t =
      fun e ->
      match f e with
      | Ok e' -> m e'
      | Error s -> Error s
  end

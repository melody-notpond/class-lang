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

module OptionMonad = Monad(OptionMonadInst)

module ResultMonadInst =
  functor (B: sig type t end) -> struct
    type 'a t = ('a, B.t) result

    let (>>=) x f =
      match x with
      | Ok v -> f v
      | Error s -> Error s

    let return x = Ok x
  end

module ResultMonad =
  functor (B: sig type t end) -> Monad(ResultMonadInst(B))

module ReaderMonadInst =
  functor (B: sig type t end) -> struct
    type b = B.t
    type 'a t = b -> 'a

    let (>>=) f g = fun x -> g (f x) x
    let return x = fun _ -> x
  end

module ReaderMonad =
  functor (B: sig type t end) -> Monad(ReaderMonadInst(B))

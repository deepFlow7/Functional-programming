module IdMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = 'a
  let return x = x
  let bind x f = f x
end

module DeferredMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = unit -> 'a
  let return x = fun () -> x
  let bind x f = f (x ())
end
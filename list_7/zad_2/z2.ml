module type RandomMonad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val random : int t
  end

  module RS : sig 
    include RandomMonad 
    val run : int -> 'a t -> 'a
  end = struct
    type 'a t = int -> 'a * int
    let return x seed = (x, seed) 
    let run seed f = fst (f seed)
    let random a = 
      let b = 16807 * (a mod 127773) - 2836 * (a / 127773) in
      if b > 0 then (b, b) else (b + 2147483647, b + 2147483647)
    (* (x : int -> 'a * int) (f : 'a -> int -> 'b * int) -> (int -> 'b * int) *)
    let bind x f = 
      fun s1 -> let (v, s2) = x s1 in
      f v s2
  end
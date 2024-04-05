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

module Shuffle(R : RandomMonad) : sig
  val shuffle : 'a list -> 'a list R.t
end = struct
  let rec chosen_rest idx xs = 
    match xs with
    | [] -> failwith "empty list"
    | x :: xs -> 
      if idx = 0 then (x, xs)
      else let (v, xs) = chosen_rest (idx - 1) xs in
      (v, x :: xs) 
  let rec shuffle xs = 
    match xs with
    | [] -> R.return []
    | _ -> 
      R.bind 
        R.random 
        (fun r -> let idx = r mod List.length xs in
        let (hd, tl) = chosen_rest idx xs in
        R.bind (shuffle tl) (fun shuffled_tl -> R.return (hd :: shuffled_tl)))
end

(*
  module SH = Shuffle(RS);;
  RS.run 0 (SH.shuffle [1;2;3;4;5;6;7;8]);;
  RS.run 23 (SH.shuffle [1;2;3;4;5;6;7;8]);;

  RS.run 1 (RS.bind RS.random (fun x -> RS.return (RS.run x RS.random)));;
*)

module SBT(State : sig type t end) : sig
    type 'a t
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val fail : 'a t
    val flip : bool t
    val get : State.t t
    val put : State.t -> unit t
    val run : State.t -> 'a t -> 'a Seq.t
  end = struct
    type 'a t = State.t -> ('a * State.t) Seq.t
    let return x s = List.to_seq [(x, s)]
    let flip s = List.to_seq [(true, s); (false, s)]
    let get s = List.to_seq [(s, s)]
    let put s1 _ = List.to_seq [((), s1)]
    let fail _ = Seq.empty 
    let run s m = Seq.map (fun x -> fst x) (m s)
    let bind m f s = 
      Seq.flat_map (fun x -> f (fst x) (snd x)) (m s)
  end

(* ------------- Monady BT, ST z wykładu  ----------- *)

(** Obliczenia z nawrotami *)
module BT : sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  (** Brak wyniku *)
  val fail : 'a t
  (** Niedeterministyczny wybór -- zwraca true, a potem false *)
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct
  (* Obliczenie typu 'a to leniwa lista wszystkich możliwych wyników *)
  type 'a t = 'a Seq.t
  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m
  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]
  let run m = m
end

(** Monada stanu -- obliczenia z ukrytą komórką mutowalnego stanu *)
module St(State : sig type t end) : sig
  type 'a t
  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t
  (** Pobierz stan *)
  val get : State.t t
  (** Ustaw stan *)
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a
end = struct
  type 'a t = State.t -> 'a * State.t
  let return x s = (x, s)
  let bind m f s =
    let (x, s) = m s in
    f x s
  let get s = (s, s)
  let set s _ = ((), s)
  let run s m = fst (m s)
end
module type Permutation = sig 
    type key
    type t
    (* permutacja jako funkcja *)
    val apply : t -> key -> key
    (* permutacja identycznosciowa *)
    val id : t
    (* permutacja odwrotna *)
    val invert : t -> t
    (* permutacja która zmienia dwa elementy miejscami *)
    val swap : key -> key -> t
    (* złożenie permutacji (jako złożenie funkcji) *)
    val compose : t -> t -> t
    (* porównanie permutacji *)
    val compare : t -> t -> int
  end

module type S = sig 
    type t
    (* czy dany element jest generowany przez zbior *)
    val is_generated : t -> t list -> bool
  end
  
  module Make(Perm : Permutation) : S with type t = Perm.t

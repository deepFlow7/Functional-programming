module type OrderedType = sig 
    type t
    val compare : t -> t -> int
end

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
    (* czy dany element jest generowany przez zbior *)
  type t 
  val is_generated : t -> t list -> bool
end

module Make(Perm : Permutation) = struct
    type t = Perm.t
    module PermSet = Set.Make(Perm)

    let is_generated (perm : t) (perm_lst : t list) = 
      let s = PermSet.add Perm.id (PermSet.of_list perm_lst) 
        in let rec inner (perm : t) (perm_set : PermSet.t) = 
          if PermSet.mem perm perm_set then true
          else let next_set = 
            PermSet.fold (fun el acc -> PermSet.union acc 
                            (PermSet.add (Perm.invert el)
                                (PermSet.fold (fun ell acc-> 
                                                PermSet.add (Perm.compose el ell) acc )
                                            perm_set PermSet.empty)))
                          perm_set
                          perm_set
                in if compare next_set perm_set == 0 then false 
                    else inner perm next_set
        in inner perm s      

end

module type OrderedType = sig
    type t
    val compare : t -> t -> int
end

module type S = sig
    type key
    type t
    
    (** permutacja jako funkcja *)
    val apply : t -> key -> key
    
    (** permutacja identycznościowa *)
    val id : t
    
    (** permutacja odwrotna *)
    val invert : t -> t
    
    (** permutacja która tylko zamienia dwa elementy miejscami *)
    val swap : key -> key -> t
    
    (** złożenie permutacji (jako złożenie funkcji) *)
    val compose : t -> t -> t

    (** porównywanie permutacji *)
    val compare : t -> t -> int
end

module Make(Key : OrderedType) = struct
    module Ourmap = Map.Make(Key)
    type key = Key.t
    type t = key Ourmap.t * key Ourmap.t
    
    (** permutacja jako funkcja *)
    let apply (perm : t) (value : key) = 
        if Ourmap.mem value (fst perm) 
            then Ourmap.find value (fst perm)
            else value
    
    (** permutacja identycznościowa *)
    let id : t = (Ourmap.empty, Ourmap.empty)

    (** permutacja odwrotna *)
    let invert (perm : t) =
        (snd perm , fst perm)
        
    (** permutacja która tylko zamienia dwa elementy miejscami *)
    let swap (e1 : key) (e2 : key) = 
        let f = (Ourmap.add e1 e2 (Ourmap.add e2 e1 Ourmap.empty)) 
        in (f, f)
    
                
 let my_fun perm = 
    fun (k : key) (v1 : key option) (v2 : key option) -> 
        match v1,v2 with
            | None, None -> None
            | _, Some(v) -> let vi = apply perm v in
                                if (Key.compare k vi == 0) then None
                                else Some(vi)
            | Some(v), None -> Some(v)          
        
 let compose (perm1 :t) (perm2 : t) = 
    (Ourmap.merge (my_fun perm1) (fst perm1) (fst perm2),
     Ourmap.merge (my_fun (invert perm2)) (snd perm2) (snd perm1))

(*
 let compose (perm1 :t) (perm2 : t) = 
        (Ourmap.merge 
            (fun (k : key) (v1 : key option) (v2 : key option) -> 
                match v1,v2 with
                | None, None -> None
                | _, Some(v) -> let vi = apply perm1 v in
                                    if (Key.compare k vi == 0) then None
                                        else Some(vi)
                | Some(v), None -> Some(v)         
            ) 
            (fst perm1) (fst perm2),
        Ourmap.merge
            (fun (k : key) (v1 : key option) (v2 : key option) ->
                match v1,v2 with
                | None, None -> None
                | Some(v), _ -> let vi = apply (invert perm2) v in 
                                if (Key.compare k vi == 0) then None 
                                    else Some(vi)
                | None, Some(v) -> Some(v)
            )
            (snd perm1) (snd perm2)
        )
*)

    (** porównywanie permutacji *)
    let compare (perm1 : t) (perm2 : t) = Ourmap.compare Key.compare (fst perm1) (fst perm2)
end

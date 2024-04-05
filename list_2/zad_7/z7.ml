  type ltree =
    | Leaf
    | Node of ltree * int * int * ltree

  let make lt1 lt2 v = match lt1, lt2 with 
    | Leaf, Leaf -> Node (lt1, v, 1, lt2)
    | Node (_, _, d1, _), Node (_, _, d2, _) -> if d2 < d1 then Node (lt1, v, d2 + 1, lt2) else Node (lt2, v, d1 + 1, lt1)
    | Leaf, Node (_, _, d, _)  -> Node (lt2, v, d + 1, lt1)
    | Node (_, _, d, _), Leaf ->  Node (lt1, v, d + 1, lt2)

let deep lt = match lt with
  | Leaf -> 0
  | Node(_, _, d, _) -> d

let rec join lt1 lt2 = match lt1, lt2 with 
    | Leaf, _ -> lt2
    | _, Leaf -> lt1 
    | Node (l1, v1, d1, r1), Node (l2, v2, d2, r2) -> if v2 < v1 then let lt_new = join r2 lt1 
        in if deep lt_new > deep l2 then Node (lt_new, v2, d1 + 1, l2)
                                    else Node (l2, v2, deep lt_new + 1, lt_new)
                                                                else join lt2 lt1

let add v lt = join lt (make Leaf Leaf v)

let del_min lt = match lt with 
  | Leaf -> failwith "empty heap"
  | Node (l, _, _, r) -> join l r
  
let get_min lt = match lt with
  | Leaf -> failwith "empty"
  | Node (_, v, _, _) -> v
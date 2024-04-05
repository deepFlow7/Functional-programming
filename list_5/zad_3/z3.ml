type fractions = tree lazy_t
and tree = 
      | Leaf
      | Node of fractions * int * int * fractions

let rec maketree : int -> int -> int -> int -> fractions =
  fun a b c d ->
    lazy (Node (maketree a b (a + c) (b + d), a + c, b + d, maketree (a + c) (b + d) c d)) 

let value f = 
  match Lazy.force f with
  | Leaf               -> failwith "empty tree"
  | Node (_, a, b, _)  -> (a, b)

let left f =
  match Lazy.force f with
  | Leaf              -> failwith "empty tree"
  | Node (l, _, _, _) -> l

let right f =
  match Lazy.force f with
  | Leaf              -> failwith "empty tree"
  | Node (_, _, _, r) -> r

let rec fractions_list f depth =
  if depth <= 0 then []
  else  
    fractions_list (left f) (depth - 1) 
    @ ((value f) :: fractions_list (right f) (depth - 1))

let rational_numbers = maketree 0 1 1 0
(*
  fractions_list rational_numbers 3
*)
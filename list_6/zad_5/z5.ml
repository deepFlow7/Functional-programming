(*  (’a -> ’b -> (’a -> ’c) -> ’c) -> ’a -> ’b list -> (’a -> ’c) -> ’c.  *)
let rec fold_left_cps f acc xs cont =
  match xs with
  | [] -> cont acc
  | x :: xs -> f acc x (fun acc -> fold_left_cps f acc xs cont)

(*   ('a -> bool) -> 'a list -> bool  *)
let for_all p xs =
  fold_left_cps 
    (fun _ x cont -> 
      if (p x) 
        then (cont true)
      else false) 
    true 
    xs
    (fun acc -> acc)

(*   int list -> int  *)
let mult_list xs = 
  fold_left_cps 
    (fun acc x cont -> 
      if x == 0 
        then 0
      else (cont acc * x))
    1 
    xs
    (fun acc -> acc)
  
(*   int list) : bool  *)
let sorted xs =
  fold_left_cps 
    (fun acc x cont -> 
      if x <= acc 
        then false
      else (cont x)) 
    Int.min_int
    xs
    (fun acc -> true)
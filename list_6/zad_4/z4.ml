(*  (’a -> ’b -> (’a -> ’c) -> ’c) -> ’a -> ’b list -> (’a -> ’c) -> ’c.  *)
let rec fold_left_cps f acc xs cont =
  match xs with
  | [] -> cont acc
  | x :: xs -> f acc x (fun acc -> fold_left_cps f acc xs cont)

(*  ('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc   *)
let fold_left f acc xs = 
  fold_left_cps 
    (fun acc x cont -> cont (f acc x))
    acc
    xs
    (fun acc -> acc)


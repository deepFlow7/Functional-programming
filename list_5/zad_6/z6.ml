type 'a to_lazy =
  | Lazy of (unit -> 'a) 
  | Value of 'a
  | Evaluating

type 'a my_lazy = 'a to_lazy ref

let force : 'a my_lazy -> 'a =
  fun ml -> 
    match !ml with
    | Lazy f   -> 
      ml := Evaluating; 
      let v = f () in 
      ml := Value(v); 
      v 
    | Value v  -> v
    | Evaluating -> failwith "Evaluating"

let fix (f : 'a my_lazy -> 'a) : 'a my_lazy =
  let rec lazy_value = ref (Lazy (fun () -> f lazy_value)) in
  lazy_value    

type 'a lazy_list =
  | Nil
  | Cons of 'a * 'a lazy_list my_lazy

let hd : 'a lazy_list -> 'a =
  function
  | Nil -> failwith "Empty list"
  | Cons(x, _) -> x

let tl : 'a lazy_list -> 'a lazy_list =
  function
  | Nil -> failwith "Empty list"
  | Cons(_, xs) -> force xs

let rec nth : 'a lazy_list -> int -> 'a = 
  fun ll n -> 
    if n < 0 then failwith "Index out of bounds" 
    else 
      if n == 0 then hd ll
      else nth (tl ll) (n - 1)


let stream_of_ones : int lazy_list my_lazy = 
  fix (fun stream_of_ones -> Cons (1, stream_of_ones))

(*
  nth (force stream_of_ones) 5   
*)

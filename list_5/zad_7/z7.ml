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

(* z7 ---------------------------------------------------- *)

type 'a lazy_list =
  | Nil
  | Cons of 'a * 'a lazy_list my_lazy

let rec nth xs n =
  match xs with
  | Nil -> raise Not_found
  | Cons(x, xs) ->
    if n = 0 then x
    else nth (force xs) (n-1)

let rec filter (p : 'a -> bool) (xs : 'a lazy_list) : ('a lazy_list) = 
  match xs with
  | Nil                  -> Nil
  | Cons(x, xs) when p x -> Cons(x, ref (Lazy (fun () -> (filter p (force xs)))))
  | Cons(_, xs)          -> filter p (force xs)

let rec take_while p xs =
  match xs with
  | Cons(x, xs) when p x -> Cons(x, ref (Lazy (fun () -> (take_while p (force xs)))))
  | _ -> Nil

let rec for_all p xs =
  match xs with
  | Nil -> true
  | Cons(x, xs) -> p x && for_all p (force xs)

let rec nats_from n = 
  Cons(n, ref (Lazy (fun () -> (nats_from (n+1)))))
let nats = nats_from 0

let rec primes =
  Cons(2, ref (Lazy (fun () -> (filter is_prime (nats_from 3)))))
and is_prime n =
  primes
  |> take_while (fun p -> p * p <= n)
  |> for_all (fun p -> n mod p <> 0)
(* po lewej najmłodszy bit, czyli np. lista 6 elementowa -> Nil One One, bo 6 = 110_(2) *)
type 'a nlist =
  | Nil
  | One  of 'a * ('a * 'a) nlist
  | Two of 'a * 'a * ('a * 'a) nlist 
  
(* +1 *)
let rec cons : 'a. 'a -> 'a nlist -> 'a nlist =
  fun x xs ->
  match xs with
  | Nil          -> One(x, Nil)
  | One(y, xs)    -> Two(x, y, xs)
  | Two(y, z, xs) -> One (x, cons (y, z) xs)

(* zwraca parę hd, tail; tail -> -1 *)
let rec view : 'a. 'a nlist -> ('a * 'a nlist) option =
  function
  | Nil -> None
  | One(x, xs) ->
    begin match view xs with
    | None -> Some(x, Nil)
    | Some((y, z), xs) -> Some(x, Two(y, z, xs))
    end
  | Two(x, y, xs) -> Some(x, One(y, xs))

let rec nth : 'a. 'a nlist -> int -> 'a =
  fun xs n ->
  match xs with
  | Nil -> raise Not_found
  | One(x, xs) ->
    if n = 0 then x
    else let (x, y) = nth xs ((n - 1) / 2) in
    if n mod 2 = 0 then y
    else x
  | Two(x, y, xs) ->
      if n = 0 then x
      else nth (One (y, xs)) (n-1)

let hd xs = 
  match view xs with
  | None -> raise (Failure "hd")
  | Some(x, xs) -> x

let tl xs = 
  match view xs with
  | None -> raise (Failure "tl")
  | Some(x, xs) -> xs
  
let rec nlist_to_list : 'a. 'a nlist -> 'a list = 
  function xs ->
  match view xs with
  | None           -> []
  | Some (hd, tl) -> hd :: (nlist_to_list tl)

let rec nlist_of_list : 'a. 'a list -> 'a nlist = 
  function
  | [] -> Nil    
  | x :: xs -> cons x (nlist_of_list xs)

assert (nlist_to_list (nlist_of_list [1;2;3;4;5]) = [1;2;3;4;5]);;
assert( nth (nlist_of_list [0;1;2;3;4;5;6]) 4 = 4);;
(* 
  [1;2;3;4;5] -> 21_(2) 
  hd xs       -> 12_(2) 


  Nie ma długich ciągów zer, np. lista 8-elementowa [1;2;3;4;5;6;7;8] to  
      Two (1, 2, One ((3, 4), One (((5, 6), (7, 8)), Nil)))
  a wczesniej Zero (Zero (Zero (One ((((1, 2), (3, 4)), ((5, 6), (7, 8))), Nil)))) 

  Zamortyzowany czas cons i tl to O(1), wczesniej O(1) tylko jesl nie przeplatalismy cons i tl, wpp. logarytmicznie
*)

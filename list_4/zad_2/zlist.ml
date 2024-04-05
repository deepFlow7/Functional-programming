(* część listy przed kursorem w odwrotnej kolejnosci + lista za kursorem *)
type 'a zlist = 'a list * 'a list

let of_list: 'a list -> 'a zlist =
  fun xs -> ([], xs)

let to_list: 'a zlist -> 'a list =
  fun xs -> List.rev (fst xs) @ (snd xs) 

let elem: 'a zlist -> 'a option =
  fun xs -> match snd xs with
  | [] -> None
  | x :: xs -> Some(x)

let move_left: 'a zlist -> 'a zlist =
  fun (xs, ys) ->  match xs with
  | []      -> raise (Failure "Can't move")
  | x :: xs -> (xs, x :: ys)

let move_right: 'a zlist -> 'a zlist =
  fun (xs, ys) -> match ys with
  | []      -> raise (Failure "Can't move")
  | y :: ys -> (y :: xs, ys)

let insert: 'a -> 'a zlist -> 'a zlist =
  fun x (xs, ys) -> (x :: xs, ys) 

let remove: 'a zlist -> 'a zlist =
  fun (xs, ys) -> match xs with
  | []      -> raise (Failure "Can't remove")
  | x :: xs -> (xs, ys)
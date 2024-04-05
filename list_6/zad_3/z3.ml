exception Finished

let for_all : ('a -> bool) -> 'a list -> bool =
  fun p xs ->
    try
      List.fold_left 
        (fun _ x -> 
          if not (p x) 
          then raise Finished
          else true) 
      true 
      xs
    with
    | Finished -> false

let mult_list (xs : int list) : int = 
  try
    List.fold_left 
      (fun acc x -> 
        if x == 0 
        then raise Finished
        else acc * x) 
    1 
    xs
  with
  | Finished -> 0

let sorted (xs : int list) : bool =
  try
    let _ = 
      List.fold_left 
      (fun acc x -> 
        if x <= acc 
        then raise Finished
        else x) 
      Int.min_int
      xs
    in true
  with
  | Finished -> false

let my_length = List.fold_left (fun acc _ -> acc + 1) 0
let my_rev = List.fold_left (fun acc x -> x :: acc) [] 
let my_map f xs = List.fold.right (fun x acc -> f x :: acc) xs []
let my_filter f xs = List.fold_right (fun x acc -> if f x then x :: acc else acc) xs []
let my_rev_map f = List.fold_left (fun acc x -> (f x) :: acc) [] 
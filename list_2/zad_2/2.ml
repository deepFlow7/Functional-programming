let rec sublists xs = match xs with
	| [] -> [] 
	| x :: [] -> [[x]; []]
	| x :: xs -> let ys = sublists(xs)
		in List.fold_left (fun acc y -> (x :: y) :: acc) ys ys
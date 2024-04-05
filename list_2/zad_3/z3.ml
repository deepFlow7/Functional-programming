let rec sufix xs = match xs with 
			| [] -> [[]]
			| _ :: ys -> xs :: sufix ys

let rec prefix xs = match xs with 
			| [] -> [[]]
			| x :: ys -> [] :: List.map (fun y -> x :: y ) (prefix ys)
(* przez wstawianie *)
let rec insert_everywhere xs y = match xs with 
					| [] -> [[y]]
					| x :: xs -> (y :: x :: xs) :: List.map (fun z -> x :: z) (insert_everywhere xs y)
                    
                    
let rec perms xs = match xs with
			| [] -> []
			| x :: [] -> [[x]]
			| x :: xs -> let ys = perms xs in List.flatten (List.map (fun s ->  insert_everywhere s x) ys)	
            
(* przez wybór *)
let rec perms2 ls = 
	let join x xs = match xs with
			| [] -> [[x]]
			| _ -> List.map (fun ys -> x :: ys) xs
	   in let rec inner ys acc = match ys with 
		| [] -> []
		| [y] -> join y (perms2 acc)
		| y :: ys -> let zs = perms2 (acc@ys) in (join y zs) @ (inner ys (y :: acc)) 
		in inner ls []
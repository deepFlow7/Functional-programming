(* nieogonowe *)
let rec merge cmp l1 l2 = match (l1, l2) with
	| [], _ -> l2
	| _, [] -> l1
	| x :: xs, y :: ys -> 
    	if cmp x y then x :: merge cmp xs l2
                 	else y :: merge cmp ys l1
		

(* ogonowe z odwracaniem *)
let merge_tail cmp xs ys = 
    let rec _merge cmp l1 l2 acc = 
        match l1, l2 with
            | [] , [] -> acc
            | [] , h :: t | h :: t , [] -> _merge cmp  [] t (h::acc)
            | h1 :: t1 , h2 :: t2 -> 
				if (cmp h1 h2) 
            		then _merge cmp t1 l2 (h1::acc) 
            	else _merge cmp l1 t2 (h2::acc) 
    in List.rev(_merge cmp xs ys [])

let rec halve ls = match ls with
| [] -> [[]; []]
| x :: [] -> [[x]; []]
| x :: y :: ys -> let xs = halve ys in
        [(x :: List.hd xs); (y :: List.hd(List.tl xs))]

let rec mergesort cmp xs = match xs with 
	| [] -> []
	| [x] -> [x]
	| _ -> let [ys;zs] = halve xs in merge cmp (mergesort cmp ys) (mergesort cmp zs)
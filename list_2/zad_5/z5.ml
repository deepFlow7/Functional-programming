let merge_tail cmp lista1 lista2 = 
    let rec _merge cmp l1 l2 result = 
        match l1, l2 with
            | [] , [] -> result
            | [] , h :: t | h :: t , [] -> _merge cmp  [] t (h::result)
            | h1 :: t1 , h2 :: t2 -> if (cmp h1 h2)
            then _merge cmp l1 t2 (h2::result) 
            else _merge cmp t1 l2 (h1::result)  
            in _merge cmp lista1 lista2 [];;
            
let rec mergesort cmp xs = match xs with 
				| [] -> []
				| [x] -> [x]
				| _ -> let [ys;zs] = halve xs in merge_tail cmp (mergesort (fun a b -> not (cmp a b)) ys) (mergesort (fun a b -> not (cmp a b)) zs)
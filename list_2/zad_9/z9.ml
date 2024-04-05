type 'a clist = { clist : 'z. ('a -> 'z -> 'z) -> 'z -> 'z }

let cnil = { clist = fun f z -> z}

let ccons a ls = {clist = fun f z -> f a (ls.clist f z)}

let map g ls = {clist = fun f z -> ls.clist (fun a s -> f (g a) s) z}

let append xs ys = {clist = fun f z -> xs.clist f (ys.clist f z)}

let clist_to_list ls = ls.clist List.cons [] 

let rec clist_of_list xs = match xs with 
  | [] -> cnil
  | x :: xs -> ccons x (clist_of_list xs)

let prod ls1 ls2 = ls1.clist (fun a1 z1 -> (ls2.clist (fun a2 z2 -> ccons (a1,a2) z2) z1)) cnil;;
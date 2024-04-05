let zero f x = x
let succ value f x = f(value f x)
let add value1 value2 f x = value1 f (value2 f x)
let mult value1 value2 f x = value1 value2(f) x
let is_zero value = value (fun x -> cfalse) ctrue
let rec cnum_of_int n = if n == 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum value = value ((+) 1) 0;;

(*int_of_cnum (add (cnum_of_int 7) (cnum_of_int 11))*)
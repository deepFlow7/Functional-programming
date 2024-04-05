type cbool = { cbool : 'a. 'a -> 'a -> 'a }
type cnum = { cnum : 'a. ('a -> 'a) -> 'a -> 'a }

let ctrue = {cbool = (fun a b -> a)}
let cfalse = {cbool = (fun a b -> b)}
let cand cbl1 cbl2 = cbl1.cbool cbl2 cfalse
let cor cbl1 cbl2 = cbl1.cbool ctrue cbl2
let cbool_of_bool b = if b then ctrue else cfalse
let bool_of_cbool b = b.cbool true false

let zero = {cnum = (fun f x -> x)}
let succ value = {cnum = fun f x -> f(value.cnum f x)}
let add value1 value2 = {cnum = fun f x -> value1.cnum f (value2.cnum f x)}
let mult value1 value2 = {cnum = fun f x -> value1.cnum value2.cnum(f) x}
let is_zero value = value.cnum (fun x -> cfalse) ctrue
let rec cnum_of_int n = if n == 0 then zero else succ (cnum_of_int (n - 1))
let int_of_cnum value = value.cnum ((+) 1) 0
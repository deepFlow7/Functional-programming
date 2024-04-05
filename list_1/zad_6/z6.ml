let ctrue a b = a
let cfalse a b = b

let cand cbl1 cbl2 = cbl1 cbl2 cfalse
let cor cbl1 cbl2 = cbl1 ctrue cbl2

let cbool_of_bool b = if b then ctrue else cfalse
let bool_of_cbool b = b true false

(* test *)
(* bool_of_cbool (cand (cbool_of_bool true) (cbool_of_bool false));; *)


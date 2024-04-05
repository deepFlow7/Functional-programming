let hd stm = stm 0
let tl stm n = stm (n + 1)
let add stm x n = (stm n) + x 
let map f stm n = f(stm n)
let map2 f stm1 stm2 n = f (stm1 n) (stm2 n)
let replace n a s n2 = if n2 = n then a else s n2
let take_every n s n2 = s(n2 * n)


let ones n = 1
let rec powers_2 n = if n = 0 then 1 else 2 * powers_2 n - 1
let  stm5 n = 5 * n

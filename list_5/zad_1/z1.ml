let rec fix f x = f (fix f) x

let fib_f fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2)

let fib = fix fib_f

let rec fix_with_limit max f x = 
  if max = 0 then failwith "Maximum depth exceeded"
  else f (fix_with_limit (max - 1) f) x

let fib2 = fix_with_limit 2 fib_f

let ht = Hashtbl.create 100
let rec fix_memo f x = 
  if Hashtbl.mem ht x then Hashtbl.find ht x
  else let _ = Hashtbl.add ht x (f (fix_memo f) x)
in Hashtbl.find ht x

let fib3 = fix_memo fib_f
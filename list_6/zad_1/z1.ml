type ('a, 'b) format = (string -> 'b) -> (string  -> 'a)

let lit (s1 : string) : ('a, 'a) format =
  fun cont s2 -> cont (s2 ^ s1)  

let int : (int -> 'a, 'a) format =
  fun cont str i -> cont (str ^ string_of_int i)  

let str : (string -> 'a, 'a) format =
  fun cont s1 s2 -> cont (s1 ^ s2)  

let (^^) d1 d2 =
    fun cont -> d1 (d2 cont)

(*
let ksprintf (d : ('a, 'b) format) : (string -> 'b) -> 'a =
  fun cont = d cont ""
*)
let sprintf (d : ('a, string) format) : 'a =
  d (fun s -> s) ""

  
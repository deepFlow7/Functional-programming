open Proc

let rec map : ('i -> 'o) -> ('a, 'z, 'i, 'o) proc =
  fun f cont ->
    Proc.recv (fun x ->
        Proc.send (f x) (fun () -> map f cont)
      )
                
let rec filter : ('i -> bool) -> ('a, 'z, 'i, 'i) proc =
  fun f cont ->
    Proc.recv (fun x ->
        if f x then
          Proc.send x (fun () -> filter f cont)
        else
          filter f cont
      )

let rec nats_from : int -> ('a, 'z, 'i, int) proc =
  fun n cont ->
    Proc.send n (fun () -> nats_from (n + 1) cont)

let rec sieve : ('a, 'a, int, int) proc =
  fun cont -> 
    Proc.recv (fun n -> 
        Proc.send n (fun () -> (filter (fun x -> x mod n != 0) >|> sieve) cont)
        )

(* run (nats_from 2 >|> sieve >|> map string_of_int) *)


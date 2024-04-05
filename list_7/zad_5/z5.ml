type 'a regexp =
  | Eps
  | Lit of ('a -> bool)
  | Or of 'a regexp * 'a regexp
  | Cat of 'a regexp * 'a regexp
  | Star of 'a regexp

let ( +% ) r1 r2 = Or(r1, r2)
let ( *% ) r1 r2 = Cat(r1, r2)

(** Obliczenia z nawrotami *)
module BT : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  (** Brak wyniku *)
  val fail : 'a t
  (** Niedeterministyczny wybór -- zwraca true, a potem false *)
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  (* Obliczenie typu 'a to leniwa lista wszystkich możliwych wyników *)
  type 'a t = 'a Seq.t

  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

let rec match_regexp (regex : 'a regexp) (xs : 'a list) : 'a list option BT.t =  
  match regex, xs with
  | Eps, _ -> BT.return None
  | Lit _, [] -> BT.fail
  | Lit p, x :: xs -> 
    if p x then BT.return (Some xs)
    else BT.fail
  | Or (r1, r2), _ ->
    BT.bind BT.flip (fun flip_opt ->
      if flip_opt then match_regexp r1 xs 
      else match_regexp r2 xs)
  | Cat (r1, r2), _ -> 
    BT.bind (match_regexp r1 xs) (fun result -> 
      match result with
      | Some x -> match_regexp r2 x
      | None -> match_regexp r2 xs)
  | Star r, [] -> BT.return None
  | Star r, _ -> BT.bind (match_regexp r xs) (fun res ->
      match res with
        | Some ys ->  
          BT.bind BT.flip (fun flip_opt ->
          if flip_opt then BT.return (Some ys)
          else
            match_regexp (Star r) ys)
        | None -> 
          BT.return None)

(*
   List.of_seq 
         (BT.run 
             (match_regexp
                      (Lit ((<>) 'a')) 
                      ['b';'c']));;

   List.of_seq 
         (BT.run 
             (match_regexp
                      (Star (Lit ((<>) 'o'))) 
                      ['s';'a';'k';'o';'p']));;

   List.of_seq 
    (BT.run 
        (match_regexp
                (Star (Star (Lit ((<>) 'k')))) 
                ['x';'a';'k']));;
  List.of_seq 
    (BT.run 
        (match_regexp
                (Star (Star (Lit ((<>) 'b')) +% (Lit ((=) 'b') *% Lit ((=) 'a')))) 
                ['s';'a';'k';'o';'p']));;

*)

let del_dup xs =
  let rec aux acc rest =
    match rest with 
    | [] -> List.rev acc
    | x :: xs -> 
      if List.mem x acc then aux acc xs
      else aux (x :: acc) xs
    in aux [] xs

 




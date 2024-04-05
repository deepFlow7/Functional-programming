type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Empty : empty fin_type
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type
and empty = |

let rec all_values : type a. a fin_type -> a Seq.t =
  function
  | Unit -> List.to_seq [()]
  | Bool -> List.to_seq [true; false]
  | Pair (a, b) -> 
    Seq.flat_map 
      (fun x -> 
        Seq.map 
          (fun y -> (x, y)) 
          (all_values b))
      (all_values a)
  | Empty -> Seq.empty
  | Either (a, b) -> 
    let av = all_values a and bv = all_values b in
    Seq.append 
      (Seq.map (fun x -> Either.left x) av)
      (Seq.map (fun y -> Either.right y) bv)

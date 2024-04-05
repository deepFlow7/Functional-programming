type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type

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

let ub = List.of_seq (all_values (Pair(Unit, Bool)))
let () = assert (ub = [((), true); ((), false)]) 

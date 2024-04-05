module Seq = struct
  include Seq
  let rec for_all : ('a -> bool) -> 'a Seq.t -> bool =
    fun pred seq ->
      match seq () with
      | Seq.Nil -> true
      | Seq.Cons (x, xs) -> pred x && for_all pred xs
  let is_empty seq =
    match seq () with
    | Seq.Nil -> true
    | _ -> false
end

type _ fin_type =
| Unit : unit fin_type
| Bool : bool fin_type
| Pair : 'a fin_type * 'b fin_type -> ('a * 'b) fin_type
| Empty : empty fin_type
| Either : 'a fin_type * 'b fin_type -> ('a, 'b) Either.t fin_type
| Function : 'a fin_type * 'b fin_type -> ('a -> 'b) fin_type
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
      (Seq.map (fun x -> Either.Left x) av)
      (Seq.map (fun y -> Either.Right y) bv)
  | Function (a, b) -> 
    all_functions a Seq.empty (all_values a) (all_values b)
      
and equal_at : type a. a fin_type -> a -> a -> bool =
  fun ty x y ->
    match ty with
    | Unit -> x = y
    | Bool -> x = y
    | Pair (a, b) ->
      equal_at a (fst x) (fst y) && 
      equal_at b (snd x) (snd y)
    | Empty -> true 
    | Either (a, b) ->
      (match (x, y) with
      | (Left x, Left y) -> equal_at a x y
      | (Right x, Right y) -> equal_at b x y
      | _ -> false)
    | Function (a, b) ->
      let all_inputs = all_values a in
      Seq.for_all (fun input -> equal_at b (x input) (y input)) all_inputs

and all_functions : type a b. a fin_type -> (a -> b) Seq.t -> a Seq.t -> b Seq.t -> (a -> b) Seq.t  =
  fun t seq a b ->
  match a () with
  | Seq.Nil -> seq
  | Seq.Cons (a, xs) -> 
    let seq =
    if Seq.is_empty seq then
      Seq.map (fun b -> fun _ -> b) b
    else 
      Seq.flat_map 
        (fun f -> 
          Seq.map (fun b -> 
            fun x -> if (equal_at t x a) then b
            else f x) b)
        seq
    in all_functions t seq xs b 

(* test *)
let xs = all_values (Function(Bool, Bool))
let ys = Seq.map (fun x -> List.map (fun y -> x y) (List.of_seq (all_values Bool))) xs 
let ls = List.of_seq ys

 


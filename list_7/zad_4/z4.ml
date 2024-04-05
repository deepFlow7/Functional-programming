module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Err : sig
  include Monad
  val fail : 'a t
  val catch : 'a t -> (unit -> 'a t) -> 'a t
  val run : 'a t -> 'a option
end = struct
  type 'a ans = 'a option
  type 'a t = { run :  ('a -> 'a ans) -> 'a ans }

  let run m = m.run (fun a -> Some a)
  let return x = {run = fun cont -> cont x}
  let fail = {run = fun _ -> None}

  let bind m f =
    { run = fun cont ->
        match run m with
        | None -> None
        | Some x -> (f x).run cont }

  let catch m f =
  { run = fun cont ->
    match run m with
    | Some x -> Some x
    | None -> (f ()).run cont }

end

module BT : sig
  include Monad
  val fail : 'a t
  val flip : bool t
  val run : 'a t -> 'a Seq.t
end = struct
  type 'a ans = 'a Seq.t
  type 'a t = { run :  ('a -> 'a ans) -> 'a ans }

  let run m = m.run (fun a -> List.to_seq [a])

  let return x = {run = fun cont -> cont x}
  let fail = {run = fun _ -> Seq.empty}

  let bind m f = { run = fun cont ->
    Seq.flat_map (fun a -> (f a).run cont) (run m) }

  let flip = { run = fun cont ->
    Seq.flat_map cont (List.to_seq [true; false])}
end

module St(State : sig type t end) : sig
  include Monad
  val get : State.t t
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a
end = struct
  type 'a ans = State.t -> 'a * State.t
  type 'a t = { run :  'r. ('a -> 'r ans) -> 'r ans }

  let run s m = fst ((m.run (fun a s2 -> (a, s2))) s)
  
  let return x = {run = fun cont -> cont x}

  let bind m f = {run = fun cont ->
    m.run (fun a -> (f a).run cont)}
    
  let set s = {run = fun cont _ -> cont () s}
  let get = {run = fun cont s -> cont s s}
end

(* Testy
  - Err -
  Err.run (Err.bind (Err.return 1) (fun x -> Err.return (x+1)));; 

  - BT -
  let (let* ) = BT.bind
  let rec select a b =
    if a >= b then BT.fail
    else
      let* c = BT.flip in
      if c then BT.return a
      else select (a+1) b
  let triples n =
    let* a = select 1 n in
    let* b = select a n in
    let* c = select b n in
    if a*a + b*b = c*c then BT.return (a, b, c)
    else BT.fail;;
  List.of_seq (BT.run (triples 30))

  - ST -
  module StInt = St(Int);;
  StInt.run 2 (StInt.return 1);;
*)
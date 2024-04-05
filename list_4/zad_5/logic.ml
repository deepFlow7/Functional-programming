type formula = 
  | Implication of formula * formula
  | Variable of string
  | MyFalse

let rec string_of_formula f =
  match f with
  | MyFalse -> "⊥"
  | Variable v -> v
  | Implication(l, r) -> match l with
    | Implication(_, _) -> "(" ^ (string_of_formula l) ^ ") => " ^ (string_of_formula r)
    | _ -> (string_of_formula l) ^ " => " ^ (string_of_formula r)

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = 
  | Theorem of formula list * formula

let assumptions thm = match thm with
  | Theorem (flist, _) -> flist
  
let consequence thm = match thm with
| Theorem (_, f) -> f

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = Theorem([f], f)

let imp_i f thm = Theorem(List.filter (fun x -> not(x = f)) (assumptions thm) ,  
          Implication (f, consequence thm))

let difference_of_lists xs ys =
  List.filter (fun x -> not (List.mem x ys)) xs

  let imp_e th1 th2 = match consequence th1 with
  | Implication(fi, f) -> if (fi = consequence th2)
      then Theorem(difference_of_lists (assumptions th1) (assumptions th2) @ assumptions th2, f)
      else failwith "not equal"
  | _ -> failwith "incorrect th1"


let bot_e f thm = match consequence thm with
  | MyFalse -> Theorem (assumptions thm, f)
  | _ -> failwith "incorrect thm"

(*
• ⊢ p → p
  let proof_1 = imp_i (Variable "p") 
                      (by_assumption (Variable "p"))
  p ⊢ p ---> ⊢ p → p
• ⊢ p → q → p
  let proof_2 = imp_i (Variable "p")
                      (imp_i (Variable "q") 
                             (by_assumption (Variable "p")))
  p ⊢ p ---> p ⊢ q → p 
        ---> p → q → p
• ⊢ (p → q → r) → (p → q) → p → r

let pq = by_assumption (Implication(Variable "p", Variable "q")) (* p => q ⊢ p => q *)
let pqr = by_assumption (Implication ((Variable "p"), (Implication(Variable "q", Variable "r")))) (* p => q => r ⊢ p => q => r *)
let qr = imp_e pqr (by_assumption (Variable "p")) (* p => q => r, p ⊢ q => r *)
let q = imp_e (by_assumption (Implication(Variable "p", Variable "q"))) (by_assumption (Variable "p")) (* p => q, p ⊢ q *)
let r = imp_e qr q (* p => q => r, p => q, p ⊢ r *)
let im_1 = imp_i (Variable "p") r (* p => q => r, p => q ⊢ p => r *)
let im_2 = imp_i (Implication (Variable "p", Variable "q")) im_1 (* p => q => r ⊢ (p => q) => p => r *)
let proof_3 = imp_i (Implication (Variable "p", Implication (Variable "q", Variable "r"))) im_2

• ⊢ ⊥ → p
let proof_4 = imp_i MyFalse
                    (bot_e (Variable "p") 
                            (by_assumption MyFalse))

    ⊥ ⊢ ⊥ ---> ⊥ ⊢ p, 
               ⊥ ⊢ ⊥ ---> ⊢ ⊥ → p
*)


(*
open Logic;;
#install_printer pp_print_formula ;;
#install_printer pp_print_theorem ;;   
*)


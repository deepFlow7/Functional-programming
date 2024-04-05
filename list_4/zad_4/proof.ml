open Logic

type lbl_assump = (string * Logic.formula)
type incomplete_proof = 
  | Goal of lbl_assump list * Logic.formula
  | IBot_e of lbl_assump list * Logic.formula * incomplete_proof
  | IImp_e of lbl_assump list * Logic.formula * incomplete_proof * incomplete_proof
  | IImp_i of lbl_assump list * Logic.formula * incomplete_proof
  | IComplete of Logic.theorem

type context = 
  | CEmpty
  | CGoal of lbl_assump list * Logic.formula
  | CBot_e of context * lbl_assump list * Logic.formula 
  | CImp_e of context * lbl_assump list * Logic.formula * incomplete_proof
  | CImp_i of context * lbl_assump list * Logic.formula 
  | CComplete of Logic.theorem

type goal = lbl_assump list * Logic.formula
type proof = 
  | Complete of Logic.theorem
  | Incomplete of goal * context

let proof g f =
  Incomplete ((g, f), CEmpty)

let qed pf =
  match pf with
  | Complete(t) -> t
  | _ -> failwith "incompleted proof"

let rec goal pf =
  match pf with
  | Complete(t) -> None
  | Incomplete(ip, _) -> 
    Some(ip)


let rec goal_down incomplete_proof ctx = 
  match incomplete_proof with
  | Goal (g, f) -> Some((g, f), ctx)
  | IBot_e (g, f, i) -> goal_down i (CBot_e(ctx, g, f))
  | IImp_e (g, f, i1, i2) -> 
  begin
    match i1 with 
    | IComplete _ ->  goal_down i2 (CImp_e(ctx, g, f, i1))
    | _ -> goal_down i1 (CImp_e(ctx, g, f, i2))
  end
  | IImp_i (g, f, i) -> goal_down i (CImp_i(ctx, g, f))
  | IComplete (_) -> None

let rec goal_up incomplete_proof ctx = 
  match ctx with
  | CEmpty -> None
  | CGoal (g, f) -> failwith "should not reach here"
  | CBot_e (ctx, g, f) -> goal_up (IBot_e(g, f, incomplete_proof)) ctx
  | CImp_e (ctx, g, f, i2) -> 
  begin
    match i2 with 
    | IComplete (_) -> goal_up (IImp_e(g, f, incomplete_proof, i2)) ctx
    | _ -> goal_down i2 (CImp_e(ctx, g, f, incomplete_proof))
  end
  | CImp_i (ctx, g, f) -> goal_up (IImp_i(g, f, incomplete_proof)) ctx
  | CComplete (_)-> None



let next pf =
  match pf with
  | Complete _ -> pf
  | Incomplete ((asm, f), ctx) -> 
      match goal_up (Goal(asm, f)) ctx with
      | Some ((asm, f), ctx) -> Incomplete ((asm, f), ctx)
      | _ -> failwith "next failed"


let intro name pf =
  match pf with
  | Complete _ -> failwith "complete proof"
  | Incomplete ((assm, f),ctx) -> 
    match f with
    | Logic.Implication(f1, f2) -> 
      Incomplete (((name, f1)::assm, f2), 
                  CImp_i (ctx, assm, f))
    | _ -> failwith "not an implication"

let rec apply f pf =
  match pf with
  | Complete _ -> failwith "complete proof"
  | Incomplete ((assump, f'), ctx) -> 
    if f = f' then pf
    else begin match f with
    | Variable _ -> failwith "incorrect formula"
    | MyFalse -> Incomplete ((assump, MyFalse), CBot_e(ctx, assump, f'))
    | Implication (f1, f2) -> 
      match apply f2 pf with
        | Complete _ -> failwith "should not reach here"
        | Incomplete ((assump, g), cx) -> 
          Incomplete ((assump,f), 
                    CImp_e(cx, assump, g, Goal(assump, f1)))

    end

let ok th1 th2 =  
  match consequence th1 with
  | Implication(fi, f) ->  fi = consequence th2
  | _ -> false

  let rec complete_p th ctx = 
  match ctx with
  | CEmpty -> Complete th
  | CGoal (_, _) -> failwith "should not reach here"
  | CBot_e (c,a,f) -> complete_p (Logic.bot_e f th) c
  | CImp_e (c,a,f,i) -> 
    begin match i with
    | IComplete t2 ->
      if (ok th t2) then complete_p (Logic.imp_e th t2) c
      else complete_p (Logic.imp_e t2 th) c
    | _ -> let y = goal_down i (CImp_e(c, a, f, IComplete th)) in
      match y with
      | None -> failwith "complete_p failed"
      | Some ((g, f), ctx) -> Incomplete((g, f), ctx)
    end
  | CImp_i (c,a,f) -> 
    begin match f with
    | Implication(f1, f2) -> complete_p (imp_i f1 th) c
    | _ -> failwith "not an implication"
  end
  | CComplete t -> Complete t

let apply_thm thm pf =
  let x = apply (Logic.consequence thm) pf in 
  match x with
  | Complete _ -> failwith "should not reach here"
  | Incomplete ((asm, f), ctx) -> 
    complete_p thm ctx
    

let apply_assm name pf =
  apply_thm (by_assumption (Variable name)) pf

let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      Logic.pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    Logic.pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()

(*
  open Logic;;
  open Proof;;
  #install_printer pp_print_formula;;
  #install_printer pp_print_theorem;;
  #install_printer pp_print_proof;;

  let p =Proof.proof [] (Implication(MyFalse, MyFalse));;
  let y = Proof.intro "y" p;;

let x = Implication(Variable("p"), Implication(Implication(Variable "p", Variable "q"), Variable "q"));;

*)

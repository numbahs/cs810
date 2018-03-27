open Ast
open Subs

type  unif_result = UOk of Subs.subst | UError of texpr*texpr

let rec mgu_help (lst:(texpr*texpr) list) (unif_lst:subst list):unif_result = 
  match lst with
  | [] -> UOk (join unif_lst)
  | (t1, t2)::tl when t1 = t2 -> mgu_help tl unif_lst
  | (FuncType(t1, t2), FuncType(t3, t4))::tl -> mgu_help ((t1,t3)::(t2,t4)::tl) unif_lst
  | (VarType s1, VarType s2)::tl when s1 = s2 -> mgu_help tl unif_lst
  | (VarType s, t1)::tl | (t1, VarType s)::tl -> 
    if SetStr.mem s @@ fv_of_type t1
    then UError((VarType s), t1)
    else
      let sub1 = create () in
      extend sub1 s t1;
      mgu_help (List.map (fun (x, y) -> (apply_to_texpr sub1 x, apply_to_texpr sub1 y)) tl) (sub1::unif_lst)
  | (RefType t1, RefType t2)::tl->
    mgu_help ((t1,t2)::tl) unif_lst
  | (t1, t2)::tl -> UError(t1,t2)

let mgu (lst:(texpr*texpr) list):unif_result = 
  mgu_help lst []

let string_of_mgu = function
  | UOk (s) -> "Ok " ^ string_of_subs s
  | UError (t1, t2) -> "Error (" ^ string_of_texpr t1 ^ ", " ^ string_of_texpr t2 ^ ")"
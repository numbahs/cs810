open Ast
open Subs

type  unif_result = UOk of Subs.subst | UError of texpr*texpr

let rec mgu (lst:(texpr*texpr) list):unif_result = 
  match lst with
  | [] -> UOk (create())
  | hd::tl -> 
    (match hd with
     | (t1, t2) when t1 = t2 -> mgu tl
     | (FuncType(t1, t2), FuncType(t3, t4)) -> mgu ((t1,t3)::(t2,t4)::tl)
     | (VarType s, t1) | (t1, VarType s) ->
       let fvl =  fv_of_type t1 in
       if SetStr.mem s fvl
       then UError((VarType s), t1)
       else 
         (* (match mgu tl with
            | UOk (sub) -> 
            (match lookup sub s with
             | None -> 
               extend sub s t1;
               UOk(sub)
             | Some (t2) -> 
            | UError (t1, t2) -> UError (t1, t2)) *)
     | (RefType t1, RefType t2) ->
       mgu ((t1,t2)::tl)
     | (t1, t2) -> UError(t1,t2))
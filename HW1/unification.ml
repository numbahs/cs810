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
     | (VarType s1, VarType s2) when s1 = s2 -> mgu tl
     | (VarType s, t1) | (t1, VarType s) -> 
       if SetStr.mem s @@ fv_of_type t1
       then UError((VarType s), t1)
       else 
         let sub1 = create () in
         extend sub1 s t1;
         (match mgu @@ List.map (fun (x, y) -> (apply_to_texpr sub1 x, apply_to_texpr sub1 y)) tl with 
          | UOk (sub2) -> UOk(join [sub1;sub2])
          | e -> e)
     | (RefType t1, RefType t2) ->
       mgu ((t1,t2)::tl)
     | (t1, t2) -> UError(t1,t2))

let string_of_mgu = function
  | UOk (s) -> "Ok " ^ string_of_subs s
  | UError (t1, t2) -> "Error (" ^ string_of_texpr t1 ^ ", " ^ string_of_texpr t2 ^ ")"
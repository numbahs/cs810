open Ast

type subst = (string,texpr) Hashtbl.t

let create ():subst = Hashtbl.create 50

let extend (s:subst) (x:string) (t:texpr):unit = Hashtbl.add s x t

let remove (s:subst) (x:string):unit = Hashtbl.remove s x

let lookup (s:subst) (x:string):(Ast.texpr option) = Hashtbl.find_opt s x

let rec apply_to_texpr (s:subst) = function
  | VarType x -> 
    (match lookup s x with
     | None -> VarType x
     | Some (t1) -> t1)
  | FuncType(t1,t2) -> FuncType((apply_to_texpr s t1), (apply_to_texpr s t2))
  | RefType(t1) -> RefType(apply_to_texpr s t1)
  | t1 -> t1

(* let rec apply_to_expr (s:subst)  *)
let apply_to_env (s1:subst) (s2:subst):unit = 
  Hashtbl.iter (fun k t1 -> 
      let look = string_of_varType t1 in
      match lookup s2 look with
      | None -> extend s2 k t1
      | Some (t2) -> remove s2 look; extend s2 k t2) s1

let key_value (s: subst) = Hashtbl.fold(fun k v acc -> (k, v)::acc) s []

let domain (sub: subst): string list = 
  Hashtbl.fold(fun k v acc -> (k)::acc) sub []

let rec join = function
  | [] -> (create ())
  | [hd] -> hd
  | hd1::hd2::tl -> 
    apply_to_env hd1 hd2;
    join @@ hd2::tl


let rec string_of_subst_pairs = function
  | [] -> "empty"
  | [(k, v)] -> k ^ ":=" ^ (Ast.string_of_texpr v)
  | (k, v)::rest -> k ^ ":=" ^ (Ast.string_of_texpr v) ^ ", " ^ (string_of_subst_pairs rest) 

let string_of_subs (t:subst):string = (string_of_subst_pairs @@ Hashtbl.fold(fun k v acc -> (k, v)::acc) t []) 

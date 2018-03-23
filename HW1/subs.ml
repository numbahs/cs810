open Ast

type subst = (string,texpr) Hashtbl.t

let create:subst = Hashtbl.create 50

let extend (s:subst) (x:string) (t:texpr):unit = Hashtbl.add s x t

let remove (s:subst) (x:string):unit = Hashtbl.remove s x

let lookup (s:subst) (x:string):(Ast.texpr option) = Hashtbl.find_opt s x

let rec string_of_subst_pairs = function
  | [] -> ""
  | [(k, v)] -> "(" ^ k ^ ", " ^ (Ast.string_of_texpr v) ^ ")"
  | (k, v)::rest -> "(" ^ k ^ ", " ^ (Ast.string_of_texpr v) ^ "), " ^ (string_of_subst_pairs rest)

let string_of_subs t = string_of_subst_pairs @@ Hashtbl.fold(fun k v acc -> (k, v)::acc) t []

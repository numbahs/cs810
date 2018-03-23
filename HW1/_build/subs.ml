open Ast

type subst = (string,Ast.texpr) Hashtbl.t

let rec string_of_subst_pairs = function
    | [] -> ""
    | [(k, v)] -> "(" ^ k ^ ", " ^ (Ast.string_of_texpr v) ^ ")"
    | (k, v)::rest -> "(" ^ k ^ ", " ^ (Ast.string_of_texpr v) ^ "), " ^ (string_of_subst_pairs rest)

let string_of_subs t = string_of_subst_pairs @@ Hashtbl.fold(fun k v acc -> (k, v)::acc) t []


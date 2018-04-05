open Ast

type subst = (string,texpr) Hashtbl.t

let create ():subst = Hashtbl.create 50

let extend (s:subst) (x:string) (t:texpr):unit = Hashtbl.add s x t

let remove (s:subst) (x:string):unit = Hashtbl.remove s x

let lookup (s:subst) (x:string):(Ast.texpr option) = Hashtbl.find_opt s x

let replace (s:subst) (x:string) (t:texpr):unit = Hashtbl.replace s x t

let remove_all (subs: subst list) (x: string):unit =
  List.iter (fun sub -> remove sub x) subs

let rec apply_to_texpr (s:subst) = function 
  | VarType x -> 
    (match lookup s x with
     | None -> VarType x
     | Some (t1) -> t1)
  | FuncType(t1,t2) -> FuncType(apply_to_texpr s t1, apply_to_texpr s t2)
  | RefType(t1) -> RefType(apply_to_texpr s t1)
  | t1 -> t1

let rec apply_to_expr (s:subst) = function
  | Unit -> Unit
  | Var x -> Var x
  | Int x -> Int x
  | Add(e1,e2) -> Add(apply_to_expr s e1, apply_to_expr s e2)
  | Sub(e1,e2) -> Sub(apply_to_expr s e1, apply_to_expr s e2)
  | Mul(e1,e2) -> Mul(apply_to_expr s e1, apply_to_expr s e2)
  | Div(e1,e2) -> Div(apply_to_expr s e1, apply_to_expr s e2)
  | NewRef(e) -> NewRef(apply_to_expr s e) 
  | DeRef(e) -> DeRef(apply_to_expr s e)
  | SetRef(e1,e2) -> SetRef(apply_to_expr s e1, apply_to_expr s e2)
  | Let(x,def,body) -> Let(x, apply_to_expr s def, apply_to_expr s body)
  | Proc(x, tx, z) -> Proc(x, apply_to_texpr s tx, apply_to_expr s z)
  | ProcUntyped(x,body) -> ProcUntyped(x, apply_to_expr s body)
  | App(e1,e2) -> App(apply_to_expr s e1, apply_to_expr s e2)
  | IsZero(e) -> IsZero(apply_to_expr s e)
  | ITE(e1,e2,e3) -> ITE(apply_to_expr s e1, apply_to_expr s e2, apply_to_expr s e3)
  | Letrec(t1, x, y, t2, e1, e2) -> 
    Letrec(apply_to_texpr s t1,
           x, y,
           apply_to_texpr s t2,
           apply_to_expr s e1,
           apply_to_expr s e2)
  | LetrecUntyped(x,param,def,body) ->
    LetrecUntyped(x, param,
                  apply_to_expr s def,
                  apply_to_expr s body) 
  | Set(x,rhs) -> Set(x, apply_to_expr s rhs)
  | BeginEnd(es) -> BeginEnd(List.map (apply_to_expr s) es)

let apply_to_env (s1:subst) (s2:subst):unit = 
  Hashtbl.iter (fun k t1 -> replace s2 k @@ apply_to_texpr s1 t1) s2

let apply_to_envs (s1:subst) (lst:subst list):subst list =
  List.iter (fun x -> apply_to_env s1 x) lst; lst

let merge_two_subst (s1:subst) (s2:subst):subst = 
  Hashtbl.iter (fun k t1 -> 
      match lookup s2 k with
      | None -> extend s2 k t1
      | Some (t2) -> replace s2 k @@ apply_to_texpr s1 t2) s1;
  s2

let join (lst:subst list):subst =
  List.fold_right (fun x y -> merge_two_subst x y) lst (create ()) 

let domain (sub: subst): string list = 
  Hashtbl.fold(fun k v acc -> (k)::acc) sub []

let check_two (sub1:subst) (sub2:subst):((texpr*texpr) list) = 
  (Hashtbl.fold
     (fun x t3 acc -> 
        match lookup sub2 x with
        | None -> acc
        | Some t4 -> (t3,t4)::acc) sub1 [])

let rec check_all (subs:subst list):((texpr*texpr) list) = 
  match subs with
  | hd1::tl -> 
    List.append
      (List.flatten 
         (List.map
            (fun x -> check_two x hd1) tl)) @@ check_all tl
  | _ -> []

let rec string_of_subst_pairs = function
  | [] -> "empty"
  | [(k, v)] -> k ^ ":=" ^ (Ast.string_of_texpr v)
  | (k, v)::rest -> k ^ ":=" ^ (Ast.string_of_texpr v) ^ ", " ^ (string_of_subst_pairs rest) 

let string_of_subs (t:subst):string = (string_of_subst_pairs @@ Hashtbl.fold(fun k v acc -> (k, v)::acc) t []) 


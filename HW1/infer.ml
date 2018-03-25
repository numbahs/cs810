open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit  -> OK(n, (create (), e, UnitType))
  | Var s -> let sub = (create ()) in
    begin
      extend sub s IntType;
      OK(n, (sub, e, VarType s))
    end
  | Int n -> OK(n, (create (), e, IntType))
  | Add(e1,e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) -> 
    (match (infer' e1 n, infer' e2 n) with
     | (OK(_, (sub1, _, x)), OK(_, (sub2, _, y))) -> 
       (match (x,y) with
        | (VarType s1, VarType s2) -> 
          OK(n, (create (), e, IntType))
        | (VarType s1, IntType) | (IntType, VarType s1) -> 
          OK(n, (create (), e, IntType))
        | _ -> Error("args to operations must evaluate to integers"))
     | (Error x, _) | (_, Error x) -> Error(x))
  | NewRef(e) -> failwith "undefined"
  | DeRef(e) -> failwith "undefined"
  | SetRef(e1,e2) -> failwith "undefined"
  | Let(x,def,body) -> failwith "undefined"
  | Proc(x,t,body) -> failwith "undefined"
  | ProcUntyped(x,body) -> failwith "undefined"
  | App(e1,e2) -> failwith "undefined"
  | IsZero(e) -> failwith "undefined"
  | ITE(e1,e2,e3) -> failwith "undefined"
  | Letrec(tRes,x,param,tPara, def,body) -> failwith "undefined"
  | LetrecUntyped(x,param,def,body) -> failwith "undefined"
  | Set(x,rhs) -> failwith "undefined"
  | BeginEnd(es) -> failwith "undefined"
and op (e1: expr) (e2: expr) = 

  let string_of_typing_judgement (sub, expr, texpr) = 
    "(" ^ (Subs.string_of_subs sub) ^ "), " ^ (Ast.string_of_expr expr) ^ ", " ^ (Ast.string_of_texpr texpr) ^ ")"


let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^ s



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type

let test (n:int) : string =
  Examples.expr n |> parse |> infer_type

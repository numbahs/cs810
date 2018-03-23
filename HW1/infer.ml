open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit  -> failwith "undefined"
  | Var s -> failwith "undefined"
  | Int n -> failwith "undefined"
  | Add(e1,e2) -> failwith "undefined"
  | Sub(e1,e2) -> failwith "undefined"
  | Mul(e1,e2) -> failwith "undefined"
  | Div(e1,e2) -> failwith "undefined"
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
  | _ -> failwith "infer: undefined"

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

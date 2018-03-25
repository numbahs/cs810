open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit  -> OK(n, (create (), e, UnitType))
  | Int n -> OK(n, (create (), e, IntType))
  | Var s -> 
    let sub = (create ())
    and ftn = "_V" ^ string_of_int (n) in
    let ft = VarType ftn in
    extend sub s ft;
    OK(n + 1, (sub, e, ft))
  | Add(e1,e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, _, t2)) -> 
          (match mgu [(t1, IntType);(t2, IntType)] with
           | UOk sub -> 
             apply_to_env tc1 tc2;
             apply_to_env tc2 sub;
             OK(n2, (sub, e, IntType))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
        | Error s -> Error s)
     | Error s -> Error s)
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

let string_of_typing_judgement (s,e,t) =
  "\027[31m "^string_of_subs s^"\027[37m |- \027[34m"^string_of_expr e
  ^": \027[30m "^string_of_texpr t

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

let subst_tests = function
  | 1 -> string_of_mgu @@ mgu [(VarType "x", VarType "_V0")]
  | 2 -> string_of_mgu @@ mgu []
  | 3 -> string_of_mgu @@ mgu [(VarType "_V0", IntType); (VarType "_V1", IntType)]
  | n -> failwith "Oops"

let print_tests () = 
  for i=1 to 3 do
    print_string @@ subst_tests i;
    print_string "\n";
    print_string @@ inf @@ Examples.expr i;
    print_string "\n";
  done;;
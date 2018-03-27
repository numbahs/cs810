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
    and ft = VarType ("_V" ^ string_of_int (n)) in
    extend sub s ft;
    OK(n + 1, (sub, e, ft))
  | Add(e1,e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, _, t2)) -> 
          (match mgu [(t1, IntType);(t2, IntType)] with
           | UOk sub -> OK(n2, (join @@ [tc1;tc2;sub], e, IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | NewRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> OK(n1, (tc1, e, RefType t1))
     | er -> er)
  | DeRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
       (match t1 with
        | RefType x -> OK(n1, (tc1, e, x))
        | VarType x -> OK(n1, (tc1, e, VarType x))
        | t -> Error (string_of_texpr t ^ " must be a RefType or VarType"))
     | er -> er)
  | SetRef(e1,e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
       (match infer' e2 n1 with 
        | OK(n2, (tc2, _, t2)) -> 
          (match mgu [(t1, RefType t2)] with
           | UOk sub -> OK(n1 + 1, (join @@ [tc1;tc2;sub], e, UnitType))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr @@ t3) ^ " and " ^ (string_of_texpr t4)))
        | er -> er)
     | er -> er)
  | Let(x,def,body) -> failwith "undefined"
  | Proc(x,t,body) -> failwith "undefined"
  | ProcUntyped(x,body) -> failwith "undefined"
  | App(e1,e2) -> failwith "undefined"
  (* (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
     (match infer' e2 n1 with
      | OK(n2, (tc2, _, t2)) -> 
        (match mgu [(t1, FuncType(t2, VarType(string_of_int n2)))] with
         | UOk sub ->  
         | Uerror (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
      | e -> e)
     | e -> e) *)
  | IsZero(e1) -> 
    (match infer' e1 n with 
     | OK(n1, (tc1, _, t1)) -> 
       (match mgu [(t1, IntType)] with
        | UOk sub -> OK(n1, (join @@ [tc1;sub], e, BoolType))
        | UError (t2, t3) -> Error ("cannot unify " ^ (string_of_texpr t2) ^ " and " ^ (string_of_texpr t3)))
     | er -> er)
  | ITE(e1,e2,e3) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, _, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, _, t2)) -> 
          (match infer' e3 n2 with
           | OK(n3, (tc3, _, t3)) ->
             (match mgu [(t1, BoolType);(t3, t2)] with
              | UOk sub -> OK(n2, (join @@ [tc1;tc2;tc3;sub], e, t2))
              | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
           | er -> er)
        | er -> er)
     | er -> er)
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

let mgu_tests = function
  | 1 -> string_of_mgu @@ mgu [(VarType "x", VarType "_V0")]
  | 2 -> string_of_mgu @@ mgu []
  | 3 -> string_of_mgu @@ mgu [(VarType "_V0", IntType); (VarType "_V1", IntType)]
  | 4 -> string_of_mgu @@ mgu [(VarType "_V0", VarType "_V1"); (VarType "_V1", IntType)]
  | n -> failwith "Oops"

let join_tests = function
  | 1 -> string_of_subs @@ (
      let sub1 = (create ())
      and sub2 = (create ())
      and sub3 = (create ())
      in extend sub1 "u" @@ FuncType(IntType, FuncType(VarType "y", VarType "y"));
      extend sub2 "x" @@ FuncType(VarType "y", VarType "y");
      extend sub3 "z" @@ FuncType(IntType, VarType "x");
      join [sub1;sub2;sub3])
  | n -> failwith "Opps"

let print_tests () = 
  for i = 1 to 1 do
    print_string @@ string_of_int i ^ " " ^ join_tests i;
    print_string "\n";
  done;
  for i=1 to 4 do
    print_string @@ string_of_int i ^ " " ^ mgu_tests i;
    print_string "\n";
    print_string @@ string_of_int i ^ " " ^ (inf @@ Examples.expr i);
    print_string "\n";
  done;
  for i=25 to 34 do
    (* print_string @@ subst_tests i;
       print_string "\n"; *)
    print_string @@ string_of_int i ^ " " ^ (inf @@ Examples.expr i);
    print_string "\n";
  done;
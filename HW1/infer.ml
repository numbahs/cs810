open Unification
open Subs
open Ast
open Examples

type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Unit  -> OK(n, (create (), e, UnitType))
  | Int n1 -> OK(n, (create (), e, IntType))
  | Var s -> 
    let sub = (create ())
    and ft = VarType ("_V" ^ string_of_int (n)) in
    extend sub s ft;
    OK(n + 1, (sub, e, ft))
  | Add(e1,e2)  -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2], 
                                apply_to_expr sub @@ Add(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | Sub(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Sub(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | Mul(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Mul(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | Div(e1, e2) ->
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ (t1, IntType)::(t2, IntType)::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ Mul(e_ret1, e_ret2),
                                IntType))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | NewRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret, t1)) -> OK(n1, (tc1, NewRef(e_ret), RefType t1))
     | er -> er)
  | DeRef(e1) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret, t1)) -> 
       let ft = VarType("_V" ^ string_of_int n1) in
       (match mgu [(t1, RefType ft)] with
        | UOk sub -> OK(n1 + 1, (join @@ apply_to_envs sub [tc1],
                                 apply_to_expr sub @@ DeRef(e_ret),
                                 apply_to_texpr sub ft))
        | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
     | er -> er)
  | SetRef(e1,e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with 
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu [(t1, RefType t2)] with
           | UOk sub -> OK(n2, (join @@ apply_to_envs sub [tc1;tc2],
                                apply_to_expr sub @@ SetRef(e_ret1, e_ret2), 
                                UnitType))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr @@ t3) ^ " and " ^ (string_of_texpr t4)))
        | er -> er)
     | er -> er)
  | Let(x,def,body) -> 
    (match infer' def n with
     | OK(n1, (tc1, e_ret1, t1)) ->
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match mgu @@ check_all [tc1;tc2] with
           | UOk sub -> 
             remove_all [tc1;tc2] x;
             OK(n2, (join @@ apply_to_envs sub [tc1;tc2], 
                     apply_to_expr sub @@ Let(x, e_ret1, e_ret2), 
                     apply_to_texpr sub t2))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | Proc(x,t,body) -> 
    (match infer' (ProcUntyped(x, body)) n with
     | OK(n1, (tc1, e_ret1, FuncType(t1, t2))) ->
       (match mgu [(t1, t)] with
        | UOk sub -> OK(n1, (tc1, 
                             apply_to_expr sub e_ret1,
                             apply_to_texpr sub t1))
        | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
     | er -> er)
  | ProcUntyped(x,body) -> 
    (match infer' (Var x) n with
     | OK(n1, (tc1, e_ret1, t1)) ->
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret2, t2)) ->
          (match mgu @@ check_all [tc1;tc2] with
           | UOk sub -> 
             remove tc2 x;
             OK(n2, 
                (join @@ apply_to_envs sub [tc2],
                 apply_to_expr sub @@ Proc(x, t1, e_ret2), 
                 apply_to_texpr sub @@ FuncType(t1, t2)))
           | UError (t1, t2) -> Error ("cannot unify " ^ (string_of_texpr t1) ^ " and " ^ (string_of_texpr t2)))
        | er -> er)
     | er -> er)
  | App(e1,e2) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          let ft = VarType("_V" ^ string_of_int n2) in
          (match mgu @@ (t1, FuncType(t2, ft))::(check_all [tc1;tc2]) with
           | UOk sub -> OK(n2 + 1,
                           (join @@ apply_to_envs sub [tc1;tc2], 
                            apply_to_expr sub @@ App(e_ret1, e_ret2),
                            apply_to_texpr sub ft))
           | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
        | er -> er)
     | er -> er)
  | IsZero(e1) -> 
    (match infer' e1 n with 
     | OK(n1, (tc1, e_ret, t1)) -> 
       (match mgu [(t1, IntType)] with
        | UOk sub -> OK(n1, 
                        (join @@ apply_to_envs sub [tc1], 
                         apply_to_expr sub @@ IsZero(e_ret), 
                         BoolType))
        | UError (t2, t3) -> Error ("cannot unify " ^ (string_of_texpr t2) ^ " and " ^ (string_of_texpr t3)))
     | er -> er)
  | ITE(e1,e2,e3) -> 
    (match infer' e1 n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' e2 n1 with
        | OK(n2, (tc2, e_ret2, t2)) -> 
          (match infer' e3 n2 with
           | OK(n3, (tc3, e_ret3, t3)) ->
             (match mgu @@ (t1, BoolType)::(t3, t2)::(check_all [tc1;tc2;tc3]) with
              | UOk sub -> OK(n3,
                              (join @@ apply_to_envs sub [tc1;tc2;tc3],
                               apply_to_expr sub @@ ITE(e_ret1, e_ret2, e_ret3), 
                               apply_to_texpr sub t2))
              | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
           | er -> er)
        | er -> er)
     | er -> er)
  | Letrec(tRes,x,param,tPara,def,body) -> 
    (match infer' (LetrecUntyped(x, param, def, body)) n with
     | OK(n1, (tc1, (Letrec(tRes2,x2, param2, tPara2, def2, body2)), t1)) -> 
       (match mgu [(tRes2, tRes);(tPara2, tPara)] with
        | UOk sub -> OK(n1, 
                        (join @@ apply_to_envs sub [tc1], 
                         apply_to_expr sub @@ Letrec(tRes2, x2, param2, tPara2, def2, body2),
                         apply_to_texpr sub t1))
        | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
     | er -> er)
  | LetrecUntyped(f,x,def,body) -> 
    (match infer' def n with
     | OK(n1, (tc1, e_ret1, t1)) -> 
       (match infer' body n1 with
        | OK(n2, (tc2, e_ret1, t2)) -> 
          (match infer' (Var(f)) n2 with
           | OK(n3, (tc3, e_ret1, t3)) ->  
             let ft = (match lookup tc1 x with
                 | None -> VarType("_V" ^ string_of_int @@ n3)
                 | Some(tx) -> tx) in remove tc1 x;
             (match mgu @@ (t3, FuncType(ft, t1))::(check_all [tc1;tc2;tc3]) with
              | UOk sub -> 
                remove_all [tc1;tc2] f;
                OK(n3 + 1, 
                   (join @@ apply_to_envs sub [tc1;tc2], 
                    apply_to_expr sub @@ Letrec(t1, f, x, ft, def, body),
                    apply_to_texpr sub t2))
              | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))
           | er -> er)
        | er -> er)
     | er -> er)
  | Set(x,rhs) ->
    (match infer' rhs n with 
     | OK(n1, (tc1, e_ret1, t1)) -> 
       replace tc1 x t1;
       OK(n1, (tc1, Set(x, e_ret1), t1))
     | er -> er) 
  | BeginEnd(es) -> 
    let checkList = 
      List.map 
        (fun x -> 
           (match infer' x n with
            | OK(n, (tc1, e_ret1, t1)) -> OK(n, (tc1, e_ret1, t1))
            | Error(s) -> Error(s))) es in
    let filteredErrors = 
      List.filter
        (function
          | OK(_, (_, _, _)) -> false
          | Error(_) -> true) checkList in
    if (List.length filteredErrors) > 0
    then List.hd filteredErrors
    else 
      let all_subs =
        (List.map
           (function
             | OK(_, (tc, _, _)) -> tc
             | _ -> failwith "impossible") checkList)
      and end_ty = 
        (match (List.nth checkList (List.length checkList - 1)) with
         | OK(_, (_, _, t_end)) -> t_end
         | _ -> failwith "impossible") in
      (match mgu @@ check_all all_subs with
       | UOk sub -> OK(n, 
                       (join @@ apply_to_envs sub all_subs, 
                        apply_to_expr sub @@ BeginEnd(es), 
                        apply_to_texpr sub end_ty))
       | UError (t3, t4) -> Error ("cannot unify " ^ (string_of_texpr t3) ^ " and " ^ (string_of_texpr t4)))



let string_of_typing_judgement (s,e,t) =
  "\027[31m"^string_of_subs s^"\027[37m |- \027[34m"^string_of_expr e
  ^": \027[30m "^string_of_texpr t 

let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^ s

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let mgu_tests (i:int) =
  mgu_tests2 i |> mgu |> string_of_mgu

(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type

let test (n:int) : string =
  expr n |> parse |> infer_type

let mgu_tests (i:int) =
  mgu_tests2 i |> mgu |> string_of_mgu

let join_tests = function
  | 1 -> string_of_subs @@ 
    (let sub1 = (create ())
     and sub2 = (create ())
     and sub3 = (create ())
     in extend sub1 "u" @@ FuncType(IntType, FuncType(VarType "y", VarType "y"));
     extend sub2 "x" @@ FuncType(VarType "y", VarType "y");
     extend sub3 "z" @@ FuncType(IntType, VarType "x");
     join [sub1;sub2;sub3])
  | 2 -> string_of_subs @@ 
    (let sub1 = create()
     and sub2 = create()
     and sub3 = create()
     in extend sub1 "x" @@ (VarType "_V0");
     extend sub3 "_V0" IntType;
     join[sub1;sub2;sub3])
  | n -> failwith "Opps"

let print_tests () = 
  for i = 1 to 2 do
    print_string @@ string_of_int i ^ " " ^ join_tests i;
    print_string "\n";
  done;
  for i=1 to 8 do
    print_string @@ string_of_int i ^ " " ^ mgu_tests i;
    print_string "\n";
  done;
  for i=1 to 26 do
    print_string @@ "\027[30m" ^ string_of_int i ^ " " ^ (inf @@ Examples.expr i);
    print_string "\n";
  done;
  print_string @@ "---------------------------------------------------- SHOULD FAIL -------------------------------------------------------------";
  print_string "\n";
  for i=1 to 10 do
    print_string @@ "\027[30m" ^ string_of_int i ^ " " ^ (inf @@ Examples.should_fail i);
    print_string "\n";
  done;
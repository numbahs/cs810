open Ast
open Subs

let expr_cases = function
  | 1 -> "x"
  | 2 -> "0"
  | 3 -> "x-1"
  | 4 -> "zero?(x)"
  | 5 -> "(f x)"
  | 6 -> "(f 0)"
  | 7 -> "(f 0)+1"
  | 8 -> "((f 0) 1)"
  | 9 -> "(((f x) y) z)"
  | 10 -> "(f ((x y) z))"
  | 11 -> "(f zero?(x))"
  | 12 -> "proc(x) { x }"
  | 13 -> "(proc (x) { x } y)"
  | 14 -> "proc (s) { proc (x) { proc (y) { ((s x) y) }}}"
  | 15 -> "proc(s) { proc (x) { proc (y) { (s (x y)) }}}"
  | 16 -> "
let x = 7  
in let y = 2 
   in let y = let x = x-1 
              in x-y 
      in (x-8)- y"
  | 17 -> "
  let g = 
     let counter = newref(0) 
     in proc (d:int) {
         begin
          setref(counter, deref(counter)+1);
          deref(counter)
         end
       }
  in (g 11) - (g 22)"
  | 18 -> "let p = proc(x:int) { 5-x } in (p 3)"
  | 19 ->"
letrec fact (x) = if zero?(x) then 1 else x*(fact (x-1)) 
in (fact 7)"
  | 20 -> "
letrec infiniteLoop (x) = (infiniteLoop (x+1)) 
in let f = proc (z) { 11 }
in (f (infiniteLoop 0))"
  | 21 -> "if zero?(0) then 1 else 2"
  | 22 -> "if zero?(x) then y else z"
  | 23 -> "newref(1)"
  | 24 -> "deref(newref(1))"
  | 25 -> "setref(newref(1), 1)"
  | 26 -> "(f (f x))"
  | 27 -> "
letrec toZero(x:int):int = (if zero?(x) then (toZero (x-1)) else x)
in (toZero 10)"
  | n -> failwith @@ "Case " ^string_of_int  n ^ " is not defined"

let fail_cases = function
  | 1 -> "(0 f)"
  | 2 -> "(f (0 2))"
  | 3 -> "(f ((zero?(x) y) z))"
  | 4 -> "(x x)"
  | 5 -> "if zero?(x) then 1 else zero?(x)"
  | 6 -> "newref(deref(1))"
  | 7 -> "deref(1)"
  | 8 -> "setref(1, 1)"
  | 9 -> "setref(newref(1), newref(1))"
  | 10 -> "if zero?(x) then x else zero?(x)"
  | n -> failwith @@ "Case " ^string_of_int  n ^ " is not defined"

let join_cases = function
  | 1 -> 
    (let sub1 = (create ())
     and sub2 = (create ())
     and sub3 = (create ())
     in extend sub1 "u" @@ FuncType(IntType, FuncType(VarType "y", VarType "y"));
     extend sub2 "x" @@ FuncType(VarType "y", VarType "y");
     extend sub3 "z" @@ FuncType(IntType, VarType "x");
     join [sub1;sub2;sub3])
  | 2 ->
    (let sub1 = create()
     and sub2 = create()
     and sub3 = create()
     in extend sub1 "x" @@ (VarType "_V0");
     extend sub3 "_V0" IntType;
     join[sub1;sub2;sub3])
  | n -> failwith @@ "Case " ^string_of_int  n ^ " is not defined"

let mgu_cases = function
  | 1 -> [(VarType "x", VarType "_V0")]
  | 2 -> []
  | 3 -> [(VarType "_V0", IntType); (VarType "_V1", IntType)]
  | 4 -> [(VarType "_V0", VarType "_V1"); (VarType "_V1", IntType)]
  (* 5 Should Fail *)
  | 5 -> [(FuncType (VarType "x", FuncType(VarType "y", VarType "x")), 
           FuncType(VarType "y", FuncType(FuncType(VarType "x", IntType), VarType "x")))]
  (* 6 Should Fail *)
  | 6 -> [(IntType, FuncType(VarType "_V1", VarType "_V2"))]
  | 7 -> [(VarType "_V0", FuncType(VarType "_V1", VarType "_V2"))]
  | 8 -> [(VarType "_V0", BoolType); (VarType "_V1", VarType "_V0")]
  | 9 -> [(VarType "_V0", VarType "_V1"); (FuncType(VarType "x", UnitType), VarType "_V1"); (VarType "x", BoolType)]
  | 10 -> [(FuncType (VarType "should_be_bool", BoolType), FuncType(BoolType, BoolType))]
  | n -> failwith @@ "Case " ^string_of_int  n ^ " is not defined"



open Ast

let expr = function
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
  | n -> failwith @@ "Expression " ^string_of_int  n ^ " is not defined"

let should_fail = function
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
  | n -> failwith @@ "Expression " ^string_of_int  n ^ " is not defined"

let mgu_tests2 = function
  | 1 -> [(VarType "x", VarType "_V0")]
  | 2 -> []
  | 3 -> [(VarType "_V0", IntType); (VarType "_V1", IntType)]
  | 4 -> [(VarType "_V0", VarType "_V1"); (VarType "_V1", IntType)]
  | 5 -> [(FuncType (VarType "x", FuncType(VarType "y", VarType "x")), 
           FuncType(VarType "y", FuncType(FuncType(VarType "x", IntType), VarType "x")))]
  | 6 -> [(IntType, FuncType(VarType "_V1", VarType "_V2"))]
  | 7 -> [(VarType "_V0", FuncType(VarType "_V1", VarType "_V2"))]
  | n -> failwith "Oops"


let expr = function
  | 1 -> "x"
  | 2 -> "0"
  | 3 -> "x-1"
  | 4 -> "zero?(x)"
  | 5 -> "(f x)"
  | 6 -> "(f 0)"
  | 7 -> "(f 0)+1"
  | 8 -> "(0 f)"
  | 9 -> "((f 0) 1)"
  | 10 -> "f (0 2)"
  | 11 -> "(((f x) y) z)"
  | 12 -> "(f ((x y) z))"
  | 13 -> "(f ((zero?(x) y) z))"
  | 14 -> "(f zero?(x))"
  | 15 -> "(x x)"
  | 16 -> "proc(x) { x }"
  | 17 -> "(proc (x) { x } y)"
  | 18 -> "proc (s) { proc (x) { proc (y) { ((s x) y) }}}"
  | 19 -> "proc(s) { proc (x) { proc (y) { (s (x y)) }}}"
  | 20 -> "
let x = 7  
in let y = 2 
   in let y = let x = x-1 
              in x-y 
      in (x-8)- y"
  | 21 -> "
  let g = 
     let counter = newref(0) 
     in proc (d:int) {
         begin
          setref(counter, deref(counter)+1);
          deref(counter)
         end
       }
  in (g 11) - (g 22)"
  | 22 -> "let p = proc(x:int) { 5-x } in (p 3)"
  | 23 ->"
letrec fact (x) = if zero?(x) then 1 else x*(fact (x-1)) 
in (fact 7)"
  | 24 -> "
letrec infiniteLoop (x) = (infiniteLoop (x+1)) 
in let f = proc (z) { 11 }
in (f (infiniteLoop 0))"
  | 25 -> "if zero?(0) then 1 else 2"
  | 26 -> "if zero?(x) then y else z"
  | 27 -> "newref(1)"
  | 28 -> "deref(newref(1))"
  | 29 -> "setref(newref(1), 1)"
  | 30 -> "if zero?(x) then 1 else zero?(x)"
  | 31 -> "newref(deref(1))"
  | 32 -> "deref(1)"
  | 33 -> "setref(1, 1)"
  | 34 -> "setref(newref(1), newref(1))"
  | n -> failwith @@ "Expression " ^string_of_int  n ^ " is not defined"

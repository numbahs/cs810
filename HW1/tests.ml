open Infer
open Unification
open Subs
open Examples

(* Interpret an expression string *)
let inf (s:string) : string =
  s |> parse |> infer_type

(* Testing expression inference *)
let expr_test (n:int) : string =
  expr_cases n |> inf 

(* Expression inference tests that should fail *)
let fail_test (n:int) : string =
  fail_cases n |> inf 

(* Testing mgu unification *)
let mgu_test (n:int) : string =
  mgu_cases n |> mgu |> string_of_mgu

(* Testing joining of type env *)
let join_test (n:int) : string = 
  join_cases n |> string_of_subs

(* Helper func to print tests *)
let test_helper (fn) (str: string) (n:int) : unit =
  Printf.printf "---| %s Tests |---\n" str;
  for i = 1 to n do
    Printf.printf ("\027[30m %d: \027[32m%s\n\027[39m") i (fn i) ;
  done;;

let run_tests () : unit = 
  test_helper expr_test "Expression" 26;
  test_helper fail_test "Fail" 10;
  test_helper join_test "Join" 2;
  test_helper mgu_test "MGU" 7;;
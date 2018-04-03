open Infer
open Unification
open Subs
open Examples

(* Interpret an expression string *)
let inf (s:string) : string =
  s |> parse |> infer_type

(* Testing expression inference *)
let expr_test (n:int) : unit =
  expr_cases n |> inf |> print_string

(* Testing mgu unification *)
let mgu_test (n:int) =
  mgu_cases n |> mgu |> string_of_mgu

(* Testing joining of type env *)
let join_test (n:int) = 
  join_cases n |> string_of_subs

(* let print_tests () = 
   for i = 1 to 2 do
    print_string @@ string_of_int i ^ " " ^ join_tests i;
    print_string "\n";
   done;
   for i=1 to 7 do
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
   done; *)

(* let run_tests = function 
   | "test" -> print_string @@ inf "0"
   | _ -> failwith "you goofed" *)
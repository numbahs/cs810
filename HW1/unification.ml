open Ast

type  unif_result = UOk of Subs.subst | UError of texpr*texpr

let mgu (lst:(texpr*texpr) list):unif_result = failwith "Opps"
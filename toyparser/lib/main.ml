open Ast

(* parse : string -> ast *)
let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

type result = int option

let string_of_result = function
  | Some n -> string_of_int n
  | None -> "None"

(* eval : ast -> result *)

let rec eval = function
  | Const(n) -> Some n
  | Add(e1, e2) -> 
      (match eval e1, eval e2 with
       | Some v1, Some v2 -> Some (v1 + v2)
       | _, _ -> None)
  | Minus(e1, e2) -> 
      (match eval e1, eval e2 with
       | Some v1, Some v2 -> Some (v1 - v2)
       | _, _ -> None)
  | Div(e1, e2) -> 
        (match eval e1, eval e2 with
         | Some v1, Some v2 -> if v2 <> 0 then Some (v1 / v2) else None
         | _, _ -> None)
  | Mul(e1, e2) -> 
        (match eval e1, eval e2 with
         | Some v1, Some v2 -> Some (v1 * v2)
         | _, _ -> None)
  |Neg(e1) ->  (match eval e1 with
      | Some v1 -> Some (-v1)
      | _ -> None)

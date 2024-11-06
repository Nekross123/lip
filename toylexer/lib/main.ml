open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* Conta singolo token nella lista *)
let count_occurrences token l =
  List.fold_left (fun acc x -> if x = token then acc + 1 else acc) 0 l

(* Rimuove duplicati da una lista *)
let rec remove_duplicates = function
  | [] -> []
  | x :: xs -> x :: remove_duplicates (List.filter (fun y -> y <> x) xs)

  
(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n tokens =
  let unique_tokens = remove_duplicates tokens in
  let token_counts = List.map (fun token -> (token, count_occurrences token tokens)) unique_tokens in
  let ordered = List.sort (fun (_, c1) (_, c2) ->  c2-c1 ) token_counts in
  let rec fine n l =
    match (n, l) with
    | (0, _) -> []               
    | (_, []) -> []              
    | (n,x :: xs) -> x :: fine (n - 1) xs
  in

fine n ordered 
;;

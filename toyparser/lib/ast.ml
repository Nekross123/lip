type ast =
    Const of int
  | Add of ast * ast
  | Minus of ast * ast
  | Mul of ast * ast
  | Div of ast * ast
  | Neg of ast   
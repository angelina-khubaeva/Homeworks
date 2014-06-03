type Expr =
  | Sum of Expr * Expr
  | Mult of Expr * Expr
  | Num of int

let rec countExpr expr =
  match expr with
    | Mult (x, y) -> countExpr(x) * countExpr(y)
    | Sum (x, y) -> countExpr(x) + countExpr(y)
    | Num x -> x

// Дано выражение, содержащее переменную, константы, арифметические операции. 
  // Посчитать производную этого выражения по этой переменной, провести в полученном выражении для производной возможные упрощения (например, 1*x = x). 
  // Выражение можно задать через вложенные discriminated union-ы

exception Exception

type Expr =
    | Var of char
    | Constant of int 
    | Product of Expr * Expr
    | Sum of Expr * Expr
    | Dif of Expr * Expr
    | Quotient of Expr * Expr
    
let simplifySum expr1 expr2 operat =
    let selectOperator e1 e2 oper =
        if oper = '+' then Sum(expr1, expr2) 
        elif oper = '-' then Dif(e1, e2) 
        else raise Exception
    let operator = 
        if operat = '+' then (+) else (-)
    match expr1 with
        | Constant 0 -> expr2
        | Constant x ->
            match expr2 with
                | Constant y -> Constant (operator x y)
                | _ -> selectOperator expr1 expr2 operat
        | _ ->
            match expr2 with
                | Constant 0 -> expr1
                | _ -> selectOperator expr1 expr2 operat

let simplifyProduct expr1 expr2 operat =
    let selectOperator expr1 expr2 oper =
        if oper = '*' then Product(expr1, expr2) 
        elif oper = '/' then Quotient(expr1, expr2) 
        else raise Exception
    let operator = 
        if operat = '*' then (*) else (/)
    match expr1 with
        | Constant 0 -> Constant 0
        | Constant 1 -> expr2
        | Constant x ->
          match expr2 with
              | Constant y -> Constant (operator x y)
              | _ -> selectOperator expr1 expr2 operat
        | _ ->
          match expr2 with
              | Constant 0 -> Constant 0
              | Constant 1 -> expr1
              | _ -> selectOperator expr1 expr2 operat

let rec differentiation = function
    | Var c -> Constant 1 
    | Constant number -> Constant 0
    | Product (expr1, expr2) -> simplifySum (simplifyProduct <| expr1 <| (differentiation expr2) <| '*') (simplifyProduct <| (differentiation expr1) <| expr2 <| '*') '+'
    | Sum (expr1, expr2) -> simplifySum (differentiation expr1) (differentiation expr2) '+'
    | Dif (expr1, expr2) -> simplifySum (differentiation expr1) (differentiation expr2)  '-'
    | Quotient(expr1, expr2) -> Quotient (simplifySum (simplifyProduct (differentiation expr1) expr2 '*') (simplifyProduct expr1 (differentiation expr2) '*') '-', simplifyProduct expr2 expr2 '*')
     
let myExpr = Sum(Product(Var('x'), Var('y')), Product(Constant (5), Var('z')))     // xy + 5z
let myExpr1 = Sum(Product(Constant(2), Var('x')), Constant(1))

printfn "result = %A" <| differentiation myExpr1                                             // x + y + 5 


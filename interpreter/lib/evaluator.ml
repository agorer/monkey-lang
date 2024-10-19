let rec eval_statements program =
  match program with
  | [] -> failwith "Cannot evaluate an empty list of statements"
  | statement :: [] -> eval statement
  | statement :: rest ->
    let result = eval statement in
    match result with
    | Object.Return result -> result
    | _ -> eval_statements rest

and eval statement =
  match statement with
  | Ast.Expression e -> eval_expression e
  | Return statement ->
    let result = eval_expression statement.return_value in
    Object.Return result
  | _ -> failwith ("Unknown statement: " ^ (Ast.show_statement statement))

and eval_expression expr =
  match expr with
  | Ast.Integer integer -> Object.Integer integer.value
  | Boolean boolean -> Object.Boolean boolean.value
  | Prefix data ->
    let right = eval_expression data.right in eval_prefix data.token right
  | Infix data ->
    let left = eval_expression data.left in
    let right = eval_expression data.right in
    eval_infix data.token left right
  | Conditional {condition; consecuence; alternative; _} ->
    eval_conditional condition consecuence alternative
  | _ -> failwith ("Unknown expression: " ^ (Ast.show_expression expr))

and eval_prefix operator right =
  match operator with
  | Token.Not -> eval_not right
  | Token.Minus -> eval_minus right
  | _ -> failwith ("Unknown prefix operator" ^ (Token.show operator))

and eval_not right =
  match right with
  | Boolean true -> Boolean false
  | Boolean false -> Boolean true
  | _ -> failwith "Not operator should have boolean operand"
    
and eval_minus right =
  match right with
  | Integer value -> Integer (-value)
  | _ -> failwith "Minus operator should have integer operand"

and eval_infix operator left right =
  match operator with
  | Token.Plus -> eval_int_op left right Int.add
  | Token.Minus -> eval_int_op left right Int.sub
  | Token.Multiplication -> eval_int_op left right Int.mul
  | Token.Division -> eval_int_op left right Int.div
  | Token.GreaterThan -> Boolean (left > right)
  | Token.LessThan -> Boolean (left < right)
  | Token.Equal ->  Boolean (left = right)
  | Token.NotEqual -> Boolean (left <> right)
  | _ -> failwith ("Unknown infix operator" ^ (Token.show operator))

and eval_int_op left right op =
  match left, right with
  | Integer left, Integer right -> Integer (op left right)
  | _ -> failwith "Integer operator should have integer operands"

and eval_conditional condition consecuence alternative =
  let condition = eval_expression condition in
  match condition, alternative with
  | Boolean true, _ -> eval_statements consecuence.statements
  | Boolean false, Some alternative -> eval_statements alternative.statements
  | Boolean false, None -> Null
  | _ -> failwith "Conditional condition should evaluate to boolean"

let rec eval_statements program env =
  match program with
  | [] -> failwith "Cannot evaluate an empty list of statements"
  | statement :: [] ->
    let env, result = eval statement env in result, env
  | statement :: rest ->
    let env, result = eval statement env in
    match result with
    | Object.Return _ -> result, env
    | _ -> eval_statements rest env

and eval statement env =
  match statement with
  | Ast.Expression e -> env, eval_expression e env
  | Return statement ->
    let result = eval_expression statement.return_value env in
    env, (Object.Return result)
  | Let statement ->
    let value = eval_expression statement.value env in
    let env = Environment.set env statement.name.value value in
    env, value

and eval_expression expr env =
  match expr with
  | Ast.Integer integer -> Object.Integer integer.value
  | Boolean boolean -> Object.Boolean boolean.value
  | Identifier identifier -> eval_identifier identifier.value env
  | Prefix data ->
    let right = eval_expression data.right env in eval_prefix data.token right
  | Infix data ->
    let left = eval_expression data.left env in
    let right = eval_expression data.right env in
    eval_infix data.token left right
  | Conditional {condition; consecuence; alternative; _} ->
    eval_conditional condition consecuence alternative env
  | Function { parameters; body; _} -> Function { parameters; body; env }
  | Call { func; arguments; _} -> eval_call func arguments env

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

and eval_conditional condition consecuence alternative env =
  let condition = eval_expression condition env in
  match condition, alternative with
  | Boolean true, _ ->
    let result, _ = eval_statements consecuence.statements env in result
  | Boolean false, Some alternative ->
    let result, _ = eval_statements alternative.statements env in result
  | Boolean false, None -> Null
  | _ -> failwith "Conditional condition should evaluate to boolean"

and eval_identifier name env =
  let value = Environment.get env name in
  match value with
  | Some value -> value
  | None -> failwith ("Using non-existent variable: " ^ name)

and eval_call func args env =
  let func = eval_expression func env in
  match func with
  | Function func -> 
    let env = eval_args func.parameters args env in
    let result, _ = eval_statements func.body.statements env in
    unwrap_return result
  | _ -> failwith "Function call does not evaluate to function expression"

and unwrap_return result =
  match result with
  | Object.Return result -> result
  | other -> other

and eval_args parameters args env =
  match parameters, args with
  | [], [] -> env
  | param :: parameters, arg :: args ->
    let arg_value = eval_expression arg env in
    let env = Environment.set env param.value arg_value in
    eval_args parameters args env
  | _, _ -> failwith "In function call: number of params and args is different"

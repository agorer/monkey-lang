type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of function_info
  | Error of string
  | Null
and function_info = {
  parameters: Ast.identifier list;
  body: Ast.block_statement;
  env: t Environment.t;
}

let rec show obj =
  match obj with
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | Return obj -> "return: " ^ show obj
  | Function _ -> "<function>"
  | Error msg -> "Error: " ^ msg
  | Null -> "<null>"

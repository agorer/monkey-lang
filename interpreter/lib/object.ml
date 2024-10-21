module HashMap = Map.Make(String)
    
type t =
  | Integer of int
  | Boolean of bool
  | String of string
  | Array of t list
  | Hash of t HashMap.t
  | Return of t
  | Function of function_info
  | Builtin of builtin
  | Error of string
  | Null
and function_info = {
  parameters: Ast.identifier list;
  body: Ast.block_statement;
  env: t Environment.t;
}
and builtin =
  | Unary of (t -> t)
  | Binary of (t -> t -> t)

let rec show obj =
  match obj with
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | String s -> s
  | Array elements ->
    let inside =
      List.fold_left (fun acc elt -> acc ^ ", " ^ (show elt)) "" elements in
    "[" ^ (remove_first_comma inside) ^ "]"
  | Hash pairs ->
    let inside =
      HashMap.fold
        (fun key value acc -> acc ^ ", " ^ key ^ ": " ^ (show value))
        pairs 
        "" in
    "{" ^ (remove_first_comma inside) ^ "}"
  | Return obj -> "return: " ^ show obj
  | Function _ -> "<function>"
  | Builtin _ -> "<builtin function>"
  | Error msg -> "Error: " ^ msg
  | Null -> "<null>"
and remove_first_comma str =
  String.sub str 2 (String.length str - 2)

let hash obj =
  match obj with
  | Integer i -> "Integer::" ^ (string_of_int i)
  | Boolean b -> "Boolean::" ^ (string_of_bool b)
  | String s -> "String::" ^ s
  | other -> failwith ("Invalid hash parameter: " ^ (show other))
        

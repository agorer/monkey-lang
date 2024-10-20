open Object
    
let rec add_builtins env =
  let env = Environment.set env "len" (Builtin (Unary len)) in
  let env = Environment.set env "first" (Builtin (Unary first)) in
  let env = Environment.set env "last" (Builtin (Unary last)) in
  let env = Environment.set env "rest" (Builtin (Unary rest)) in
  env

and len str =
  match str with
  | Object.String str -> Object.Integer (String.length str)
  | _ -> failwith "Len: invalid argument (only strings allowed)"

and first array =
  match array with
  | Object.Array elements ->
    if not (List.is_empty elements) then List.hd elements
    else Null
  | _ -> failwith "First: invalid argument (only arrays allowed)"

and last array =
  match array with
  | Object.Array elements ->
    if not (List.is_empty elements) then
      List.nth elements (List.length elements - 1)
    else
      Null
  | _ -> failwith "Last: invalid argument (only arrays allowed)"

and rest array =
  match array with
  | Object.Array elements ->
    if List.is_empty elements then Array([])
    else Array(List.tl elements)
  | _ -> failwith "First: invalid argument (only arrays allowed)"

open Object
    
let rec add_builtins env =
  let env = Environment.set env "len" (Builtin (Unary len)) in
  env

and len str =
  match str with
  | Object.String str -> Object.Integer (String.length str)
  | _ -> failwith "Len: invalid argument (only strings allowed)"

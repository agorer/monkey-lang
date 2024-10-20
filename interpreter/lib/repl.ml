let prompt = ">> "
  
let rec start in_channel out_channel env =
  let () = Printf.fprintf out_channel "%s%!" prompt in
  let input = In_channel.input_line in_channel in
  match input with
  | None -> ()
  | Some input ->
    let lexer = Lexer.make input in
    let parser = Parser.make lexer in
    let program = Parser.parse_program parser [] in
    let result, env =
      try Evaluator.eval_statements program env with
      | Failure msg -> Object.Error msg, env in
    print_endline (Object.show result);
    start in_channel out_channel env

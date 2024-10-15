let prompt = ">> "

let rec consume lexer out_channel =
  let lexer, token = Lexer.nextToken lexer in
  match token with
  | Token.EOF -> ()
  | token ->
    Printf.fprintf out_channel "%s\n" (Token.show token);
    consume lexer out_channel
  
let rec start in_channel out_channel =
  let () = Printf.fprintf out_channel "%s%!" prompt in
  let input = In_channel.input_line in_channel in
  match input with
  | None -> ()
  | Some input ->
    let lexer = Lexer.make input in
    let () = consume lexer out_channel in
    start in_channel out_channel

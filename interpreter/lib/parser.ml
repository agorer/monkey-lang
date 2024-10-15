open Ast
    
type t = {
  lexer: Lexer.t;
  current: Token.t;
  peek: Token.t;
}

let make lexer =
  let lexer, current = Lexer.nextToken lexer in
  let lexer, peek = Lexer.nextToken lexer in
  { lexer; current; peek }

let next_token parser =
  let lexer, peek = Lexer.nextToken parser.lexer in
  { lexer; current = parser.peek; peek }, parser.peek

let skip_assign parser =
  let parser, token = next_token parser in
  match token with
  | Token.Assign -> parser
  | _ -> failwith "An assign symbol should be at this possition"

let parse_expression parser =
  (* FIXME *)
  let parser, _ = next_token parser in (* Skip value *)
  let parser, _ = next_token parser in (* Skip semicolon *)
  parser, Token.Int 5

let parse_let_statement parser =
  let let_token = parser.current in
  match parser.peek with
  | Token.Ident name ->
    let parser, ident_token = next_token parser in
    let parser = skip_assign parser in
    let parser, expression = parse_expression parser in
    parser, {
      token = let_token;
      name = { token = ident_token; value = name };
      value = expression;
    }
  | _ -> failwith "Let statement should start with identifier"

let parse_statement parser =
  match parser.current with
  | Token.Let -> parse_let_statement parser
  | _ -> failwith "Unknown statement"

let rec parse_program parser program =
  match parser.current with
  | Token.EOF -> program
  | _ ->
    (* We can improve performance on large programs by adding statements to the
       beginning of program and reversing the list in the end *)
    let parser, statement = parse_statement parser in
    let program = program @ [statement] in
    let parser, _ = next_token parser in
    parse_program parser program
  

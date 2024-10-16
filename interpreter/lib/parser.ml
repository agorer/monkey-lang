open Ast
    
type t = {
  lexer: Lexer.t;
  current: Token.t;
  peek: Token.t;
}

type precedence =
  | Lowest
  | Equals
  | Comparison
  | Sum
  | Product
  | Prefix
  | Call

let to_precedence token =
  match token with
  | Token.Equal -> Equals
  | NotEqual -> Equals
  | LessThan -> Comparison
  | GreaterThan -> Comparison
  | Plus -> Sum
  | Minus -> Sum
  | Division -> Product
  | Multiplication -> Product
  | _ -> Lowest

let make lexer =
  let lexer, current = Lexer.nextToken lexer in
  let lexer, peek = Lexer.nextToken lexer in
  { lexer; current; peek }

let next_token parser =
  let lexer, peek = Lexer.nextToken parser.lexer in
  let current = parser.peek in
  { lexer; current; peek }, current

let skip_assign parser =
  let parser, token = next_token parser in
  match token with
  | Token.Assign -> let parser, _ = next_token parser  in parser
  | _ -> failwith "An assign symbol should be at this possition"

let advance_to_semicolon parser =
  match parser.peek with
  | Semicolon -> let parser, _ = next_token parser in parser
  | _ -> parser

let rec parse_expression parser precedence =
  let prefix = prefix_func parser.current in
  match prefix with
  | Some prefix -> 
    let parser, left_exp = prefix parser in
    let parser, exp = parse_right parser left_exp precedence  in
    parser, exp
  | None -> failwith ("No prefix function for token " ^ (Token.show parser.current))

and parse_right parser left precedence =
  if parser.peek <> Token.Semicolon &&
     precedence < (to_precedence parser.peek) then
    let infix = infix_func parser.peek in
    match infix with
    | Some infix ->
      let parser, _ = next_token parser in
      let parser, left = infix parser left in
      parse_right parser left precedence
    | None -> parser, left
  else
    let parser = advance_to_semicolon parser in
    parser, left
    
and prefix_func token =
  match token with
  | Token.Ident _ -> Some parse_identifier
  | Int _ -> Some parse_integer
  | Not | Token.Minus -> Some parse_prefix
  | _ -> None

and infix_func token =
  match token with
  | Token.Plus
  | Minus
  | Division
  | Multiplication
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan -> Some parse_infix
  | _ -> None

and parse_identifier parser =
  match parser.current with
  | Ident value ->  parser, Identifier({ token = parser.current; value })
  | _ -> failwith "Expected identifier token"

and parse_integer parser =
  match parser.current with
  | Int value -> parser, Integer({ token = parser.current; value })
  | _ -> failwith "Expected integer literal token"

and parse_prefix parser =
  let token = parser.current in
  let parser, _ = next_token parser in
  let parser, right = parse_expression parser Prefix in
  
  parser, Ast.Prefix({ token; right })

and parse_infix parser left =
  let token = parser.current in
  let precedence = to_precedence token in
  let parser, _ = next_token parser in
  let parser, right = parse_expression parser precedence in
  parser, Infix({ token; left; right })

let parse_let_statement parser =
  let let_token = parser.current in
  match parser.peek with
  | Token.Ident name ->
    let parser, ident_token = next_token parser in
    let parser = skip_assign parser in
    let parser, expression = parse_expression parser Lowest in
    parser, Let({
      token = let_token;
      name = { token = ident_token; value = name };
      value = expression;
    })
  | _ -> failwith "Let statement should start with identifier"

let parse_return_statement parser =
  let return_token = parser.current in
  let parser, _ = next_token parser in
  let parser, expression = parse_expression parser Lowest in
  parser, Return({
    token = return_token;
    return_value = expression;
  })

let parse_statement parser =
  match parser.current with
  | Token.Let -> parse_let_statement parser
  | Token.Return -> parse_return_statement parser
  | _ ->
    let parser, expression = parse_expression parser Lowest in
    parser, Expression(expression)

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
  

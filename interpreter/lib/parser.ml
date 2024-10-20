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
  | Index
[@@deriving show]

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
  | LeftParen -> Call
  | LeftBracket -> Index
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

let advance_to parser token =
  if parser.peek = token then
    let parser, _ = next_token parser in parser
  else
    parser

let rec parse_statement parser =
  match parser.current with
  | Token.Let -> parse_let_statement parser
  | Token.Return -> parse_return_statement parser
  | _ ->
    let parser, expression = parse_expression parser Lowest in
    parser, Expression(expression)
      
and parse_let_statement parser =
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

and parse_return_statement parser =
  let return_token = parser.current in
  let parser, _ = next_token parser in
  let parser, expression = parse_expression parser Lowest in
  parser, Return({
    token = return_token;
    return_value = expression;
  })
    
and parse_expression parser precedence =
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
    let parser = advance_to parser Semicolon in
    parser, left
    
and prefix_func token =
  match token with
  | Token.Ident _ -> Some parse_identifier
  | Int _ -> Some parse_integer
  | True | False -> Some parse_boolean
  | String _ -> Some parse_string
  | Not | Token.Minus -> Some parse_prefix
  | LeftParen -> Some parse_group
  | If -> Some parse_if
  | Function -> Some parse_fn
  | LeftBracket -> Some parse_array
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
  | LeftParen -> Some parse_call
  | LeftBracket -> Some parse_index
  | _ -> None

and parse_block parser =
  let token = parser.current in
  let rec parse_statements parser statements =
    let parser, token = next_token parser in
    match token with
    | Token.RightBrace -> parser, statements
    | _ ->
      let parser, statement = parse_statement parser in
      let statements = statements @ [statement] in
      parse_statements parser statements
  in
  let parser, statements = parse_statements parser [] in
  parser, { token; statements }

and parse_identifier parser =
  match parser.current with
  | Ident value ->  parser, Identifier({ token = parser.current; value })
  | _ -> failwith "Expected identifier token"

and parse_integer parser =
  match parser.current with
  | Int value -> parser, Integer({ token = parser.current; value })
  | _ -> failwith "Expected integer literal token"

and parse_boolean parser =
  match parser.current with
  | True -> parser, Boolean({ token = parser.current; value = true })
  | False -> parser, Boolean({ token = parser.current; value = false })
  | _ -> failwith "Expected boolean literal token"

and parse_string parser =
  match parser.current with
  | String value -> parser, String({ token = parser.current; value})
  | _ -> failwith "Expected string literal token"

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

and parse_group parser =
  let parser, _ = next_token parser in (* skip left paren *)
  let parser, exp = parse_expression parser Lowest in
  match parser.peek with
  | Token.RightParen ->
    let parser, _ = next_token parser in (* skip right paren *)
    parser, exp
  | _ -> failwith "Expected right paren"

and parse_if parser =
  let token = parser.current in
  let parser, _ = next_token parser in (* skip if *)
  let parser, condition = parse_expression parser Lowest in
  let parser, _ = next_token parser in (* skip right paren *)
  let parser, consecuence = parse_block parser in
  let parser = advance_to parser Else in
  let parser, alternative = parse_else parser in
  parser, Conditional({ token; condition; consecuence; alternative })

and parse_else parser =
  if parser.current = Token.Else then
    let parser, _ = next_token parser in
    let parser, alternative = parse_block parser in
    parser, Some alternative
  else parser, None

and parse_fn parser =
  let token = parser.current in
  let parser, _ = next_token parser in (* skip fn *)
  let parser, parameters = parse_parameters parser [] in
  let parser, body = parse_block parser in
  parser, Function({ token; parameters; body })

and parse_parameters parser parameters =
  match parser.current with
  | Token.RightParen ->
    let parser, _ = next_token parser in (* skip right paren *)
    parser, parameters
  | _ ->
    let parser, parameter = parse_parameter parser in
    match parameter with
    | None -> parse_parameters parser parameters
    | Some parameter -> parse_parameters parser (parameters @ [parameter])

and parse_parameter parser =
  let parser, token = next_token parser in
  match token with
  | RightParen -> parser, None
  | Ident value ->
    let parser, _ = next_token parser in (* skip identifier *)
    parser, Some { token; value }
  | _ -> failwith "Expected identifier token"

and parse_call parser func =
  let token = parser.current in
  let parser, arguments = parse_expression_list parser [] in
  parser, Ast.Call({token; func; arguments})

and parse_expression_list parser expressions =
  let advance_to_separator parser =
    match parser.peek with
    | Comma | RightParen | RightBracket -> let parser, _ = next_token parser in parser
    | _ -> parser
  in
  match parser.current with
  | Token.RightParen ->
    let parser, _ = next_token parser in (* skip right paren *)
    parser, expressions
  | Token.RightBracket -> parser, expressions
  | _ ->
    let parser, token = next_token parser in (* skip separator or comma *)
    if token <> Token.RightParen && token <> Token.RightBracket then
      let parser, expression = parse_expression parser Lowest in
      let parser = advance_to_separator parser in
      parse_expression_list parser (expressions @ [expression])
    else
      parse_expression_list parser expressions

and parse_array parser =
  let token = parser.current in
  let parser, elements = parse_expression_list parser [] in
  parser, Ast.Array({token; elements})

and parse_index parser array =
  let token = parser.current in
  let parser, _ = next_token parser in (* skip left bracket *)
  let parser, index = parse_expression parser Lowest in
  let parser = advance_to parser Token.RightBracket in
  parser, Ast.Index({ token; array; index })

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

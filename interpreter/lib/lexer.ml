type t = {
  input: string;
  position: int;
  readPosition: int;
  current: char option;
}

let read_char lexer =
  let current =
    if lexer.readPosition >= (String.length lexer.input) then None
    else Some (String.get lexer.input lexer.readPosition) in
  { lexer with
    position = lexer.readPosition;
    readPosition = lexer.readPosition + 1;
    current }

let rewind_char lexer =
  let position =
    if lexer.position <= 0 then 0
    else lexer.position - 1 in
  let current = Some (String.get lexer.input position) in
  { lexer with position; readPosition = position + 1; current }

let make input =
  { input; position = -1; readPosition = 0; current = None }

let is_letter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
  
let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let read_similar lexer predicate =
  let first = lexer.position in
  let rec consume lexer =
    match lexer.current with
    | Some ch ->
      if predicate ch then
        let lexer = read_char lexer in
        consume lexer
      else lexer, lexer.position
    | None -> lexer, lexer.position in
  let lexer, last = consume lexer in
  let length = last - first in
  (rewind_char lexer), (String.sub lexer.input first length)

let rec skip_whitespace lexer =
  match lexer.current with
  | Some ch ->
    if is_whitespace ch then
      let lexer = read_char lexer in
      skip_whitespace lexer
    else lexer
  | None -> lexer

let pp_current lexer =
  match lexer.current with
  | Some ch -> print_char ch
  | None -> print_string "<EOF>"

let nextToken lexer =
  let lexer = read_char lexer in
  let lexer = skip_whitespace lexer in
  match lexer.current with
  | Some '=' -> lexer, Token.Assign
  | Some ';' -> lexer, Token.Semicolon
  | Some '(' -> lexer, Token.LeftParen
  | Some ')' -> lexer, Token.RightParen
  | Some ',' -> lexer, Token.Comma
  | Some '+' -> lexer, Token.Plus
  | Some '{' -> lexer, Token.LeftBrace
  | Some '}' -> lexer, Token.RightBrace
  | Some ch ->
    if is_letter ch then
      let lexer, identifier = read_similar lexer is_letter in
      match identifier with
      | "fn" -> lexer, Token.Function
      | "let" -> lexer, Token.Let
      | other -> lexer, Token.Ident other
    else if is_digit ch then
      let lexer, number = read_similar lexer is_digit in
      lexer, Token.Int (int_of_string number)
    else
      lexer, Token.Illegal
  | None -> lexer, Token.EOF


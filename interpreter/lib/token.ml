type token_type =
  Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  (* Delimiters *)
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  (* Keywords *)
  | Function
  | Let

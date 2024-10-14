type token_type =
  Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Product
  | Division
  | Not
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  (* Booleans *)
  | True
  | False
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
  | If
  | Else
  | Return

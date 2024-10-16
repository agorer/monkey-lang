type t =
  Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Multiplication
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
[@@deriving show]

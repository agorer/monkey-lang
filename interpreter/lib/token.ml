type t =
  Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident of string
  | Int of int
  | String of string
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
  | Colon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  (* Keywords *)
  | Function
  | Let
  | If
  | Else
  | Return
[@@deriving show]

type expression =
  | Identifier of identifier
  | Integer of integer_literal
  | Prefix of prefix_expression
  | Infix of infix_expression
and identifier = {
  token: Token.t;
  value: string
}
and integer_literal = {
  token: Token.t;
  value: int
}
and prefix_expression = {
  token: Token.t;
  right: expression;
}
and infix_expression = {
  token: Token.t;
  left: expression;
  right: expression;
}
[@@deriving show]
                    
type let_statement = {
  token: Token.t;
  name: identifier;
  value: expression;
}
[@@deriving show]

type return_statement = {
  token: Token.t;
  return_value: expression;
}
[@@deriving show]

type statement =
  | Let of let_statement
  | Return of return_statement
  | Expression of expression
[@@deriving show]

type program = statement list
[@@deriving show]

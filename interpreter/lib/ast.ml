type statement =
  | Let of let_statement
  | Return of return_statement
  | Expression of expression
and let_statement = {
  token: Token.t;
  name: identifier;
  value: expression;
}
and return_statement = {
  token: Token.t;
  return_value: expression;
}
and block_statement = {
  token: Token.t;
  statements: statement list;
}
and expression =
  | Identifier of identifier
  | Integer of integer_literal
  | Boolean of boolean_literal
  | Prefix of prefix_expression
  | Infix of infix_expression
  | Conditional of if_expression
  | Function of fn_expresssion
  | Call of fn_call_expression
and identifier = {
  token: Token.t;
  value: string
}
and integer_literal = {
  token: Token.t;
  value: int
}
and boolean_literal = {
  token: Token.t;
  value: bool
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
and if_expression = {
  token: Token.t;
  condition: expression;
  consecuence: block_statement;
  alternative: block_statement option;
}
and fn_expresssion = {
  token: Token.t;
  parameters: identifier list;
  body: block_statement;
}
and fn_call_expression = {
  token: Token.t;
  func: expression;
  arguments: expression list;
}
[@@deriving show]

type program = statement list
[@@deriving show]

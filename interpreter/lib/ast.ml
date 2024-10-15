type expression = Token.t

type identifier = {
  token: Token.t;
  value: string
}
                    
type let_statement = {
  token: Token.t;
  name: identifier;
  value: expression;
}

type statement =
  Let of let_statement

type program = statement list

let%test "should return next token" =
  let lexer = Lexer.make "=+(){},;" in
  let lexer, maybeAssign = Lexer.nextToken lexer in
  let lexer, maybePlus = Lexer.nextToken lexer in
  let lexer, maybeLeftParen = Lexer.nextToken lexer in
  let lexer, maybeRightParen = Lexer.nextToken lexer in
  let lexer, maybeLeftBrace = Lexer.nextToken lexer in
  let lexer, maybeRightBrace = Lexer.nextToken lexer in
  let lexer, maybeComma = Lexer.nextToken lexer in
  let lexer, maybeSemicolon = Lexer.nextToken lexer in
  let _, maybeEOF = Lexer.nextToken lexer in
  maybeAssign = Token.Assign &&
  maybePlus = Token.Plus &&
  maybeLeftParen = Token.LeftParen &&
  maybeRightParen = Token.RightParen &&
  maybeLeftBrace = Token.LeftBrace &&
  maybeRightBrace = Token.RightBrace &&
  maybeComma = Token.Comma &&
  maybeSemicolon = Token.Semicolon &&
  maybeEOF = Token.EOF

let%test "should tokenize assignments" =
  let lexer = Lexer.make "let five = 5;" in
  let lexer, maybeLet = Lexer.nextToken lexer in
  let lexer, maybeIdent = Lexer.nextToken lexer in
  let lexer, maybeAssign = Lexer.nextToken lexer in
  let lexer, maybeInt = Lexer.nextToken lexer in
  let _, maybeSemicolon = Lexer.nextToken lexer in
  maybeLet = Token.Let &&
  maybeIdent = Token.Ident "five" &&
  maybeAssign = Token.Assign &&
  maybeInt = Token.Int 5 &&
  maybeSemicolon = Token.Semicolon

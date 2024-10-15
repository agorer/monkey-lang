let setup input =
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  Parser.parse_program parser []

let%test "Let statements" =
  let input = "let x = 5;\
               let y = 10;" in
  let program = setup input in
  (List.length program) = 2

let%test "Return statements" =
  let input = "return 5;\
               return 6;" in
  let program = setup input in
  (List.length program) = 2

let%test "Let statements" =
  let input = "let x = 5;\
               let y = 10;" in
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  let program = Parser.parse_program parser [] in
  (List.length program) = 2

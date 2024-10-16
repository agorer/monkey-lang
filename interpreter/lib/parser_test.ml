let setup input =
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  Parser.parse_program parser []

let%test "Let statements" =
  let input = "let x = 5;" in
  let program = setup input in
  (List.length program) = 1

let%test "Return statements" =
  let input = "return 5;" in
  let program = setup input in
  (List.length program) = 1

let%test "Identifier expressions" =
  let input = "foobar;" in
  let program = setup input in
  (List.length program) = 1

let%test "Integer literal expressions" =
  let input = "5;" in
  let program = setup input in
  (List.length program) = 1

let%test "Prefix operators" =
  let input = "-5; !6" in
  let program = setup input in
  (List.length program) = 2

let%test  "Infix operators" =
  let input = "5 + 5;\
               5 - 5;\
               5 * 5;\
               5 / 5;\
               5 > 5;\
               5 < 5;\
               5 == 5;\
               5 != 5;" in
  let program = setup input in
  (List.length program) = 8

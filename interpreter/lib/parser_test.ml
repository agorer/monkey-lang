let setup input =
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  Parser.parse_program parser []

let boolean_value statement =
  match statement with
  | Ast.Expression Boolean info -> Some info.value
  | _ -> None

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

let%test "Boolean literal expressions" =
  let input = "true; false;" in
  let program = setup input in
  let first = List.hd program in
  let second = List.nth program 1 in
  (List.length program) = 2 &&
  (boolean_value first) = Some true &&
  (boolean_value second) = Some false

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

let%test "Grouped expressions" =
  let input = "2 * (5 + 6)" in
  let program = setup input in
  (List.length program) = 1

let%test "If expressions" =
  let input = "if (x < y) { x };" in
  let program = setup input in
  (List.length program) = 1

let%test "If-else expressions" =
  let input = "if (x < y) { x } else { y };" in
  let program = setup input in
  (List.length program) = 1

let%test "Function expresssions" =
  let input = "fn (x, y) { return x + y; };" in
  let program = setup input in
  (List.length program) = 1

let%test "Function calls" =
  let input = "fn (x, y) { x + y; } (1, 2);" in
  let program = setup input in
  (List.length program = 1)

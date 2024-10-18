let setup input =
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  let program = Parser.parse_program parser [] in
  Evaluator.eval_program program
    
let%test "Eval integer expression" =
  let input = "5" in
  let result = setup input in
  result = Integer 5

let%test "Eval boolean expression" =
  let input = "true" in
  let result = setup input in
  result = Boolean true

let%test "Not operator" =
  let input = "!!false" in
  let result = setup input in
  result = Boolean false

let%test "Minus operator" =
  let input = "-5" in
  let result = setup input in
  result = Integer (-5)

let%test "Sum and product operators" =
  let input = "5 + 6 * 5" in
  let result = setup input in
  result = Integer 35

let%test "Sub and division operators" =
  let input = "6 - 4 / 2" in
  let result = setup input in
  result = Integer 4

let%test "Less than operator" =
  let input = "4 < 5" in
  let result = setup input in
  result = Boolean true

let%test "Greater than operator" =
  let input = "4 > 5" in
  let result = setup input in
  result = Boolean false

let%test "Integer equality  operator" =
  let input = "5 == 5" in
  let result = setup input in
  result = Boolean true

let%test "Integer disequality operator" =
  let input = "5 != 5" in
  let result = setup input in
  result = Boolean false

let%test "Boolean equality operator" =
  let input = "true == true" in
  let result = setup input in
  result = Boolean true

let%test "Boolean disequality operator" =
  let input = "true != true" in
  let result = setup input in
  result = Boolean false

let setup input =
  let lexer = Lexer.make input in
  let parser = Parser.make lexer in
  let program = Parser.parse_program parser [] in
  let env = Environment.make () in
  let env = Builtins.add_builtins env in
  let result, _ =
    try Evaluator.eval_statements program env with
    | Failure msg -> (Object.Error msg), env in
  result
    
let%test "Eval integer expression" =
  let input = "5" in
  let result = setup input in
  result = Integer 5

let%test "Eval multiple statements" =
  let input = "5; 6;" in
  let result = setup input in
  result = Integer 6

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

let%test "If true expression" =
  let input = "if (true) { 10 }" in
  let result = setup input in
  result = Integer 10

let%test "If false expression" =
  let input = "if (false) { 10 }" in
  let result = setup input in
  result = Null

let%test "If-else true expression" =
  let input = "if (true) { 10 } else { 11 }" in
  let result = setup input in
  result = Integer 10

let%test "If-else false expression" =
  let input = "if (false) { 10 } else { 11 }" in
  let result = setup input in
  result = Integer 11

let%test "Return statement" =
  let input = "5; return 6; 7;" in
  let result = setup input in
  result = Return (Integer 6)

let%test "Nested return statement" =
  let input = "if (true) { if (true ) { return 5; } 6; }; 6;" in
  let result = setup input in
  result  = Return (Integer 5)

let%test "Error handling" =
  let input = "5 - true" in
  let result = setup input in
  result = Error "Integer operator should have integer operands"

let%test "Let statements" =
  let input = "let a = 5; 5 * a" in
  let result = setup input in
  result = Integer  25

let%test "Function call" =
  let input = "let double_sum = fn(x, y) { 2 * (x + y) }; double_sum(1, 2)" in
  let result = setup input in
  result = Integer 6

let%test "Function expression call" =
  let input = "fn(x, y) { 2 * (x + y) }(1, 2)" in
  let result = setup input in
  result = Integer 6

let%test "Function return" =
  let input = "fn(x) { return x }(1); return 2" in
  let result = setup input in
  result = Return (Integer 2)

let%test "Eval string expression" =
  let input = {|"Hello world!"|} in
  let result = setup input in
  result = String "Hello world!"

let%test "Eval string concat" =
  let input = {|"Hello" + " world!"|} in
  let result = setup input in
  result = String "Hello world!"

let%test "Builtins len" =
  let input = {|len("four")|} in
  let result =  setup input in
  result = Integer 4

let%test "Builtins len error" =
  let input = {|len(4)|} in
  let result =  setup input in
  result = Error "Len: invalid argument (only strings allowed)"

let%test "Eval array expression" =
  let input = "[1, 2]" in
  let result = setup input in
  result = Array [Integer 1; Integer 2]

let%test "Eval array indexation" =
  let input = "[1, 2][1]" in
  let result = setup input in
  result = Integer 2

let%test "Eval hash expression" =
  let input = {|{ "age": 5 }|} in
  let result = setup input in
  let open Object  in
  let expected = Hash (HashMap.of_list ["String::age", Integer 5]) in
  result = expected

let%test "Eval hash index expressions" =
  let input = {|{ "age": 5 }["age"]|} in
  let result = setup input in
  result = Integer 5

let%test "Eval not-found hash index expressions" =
  let input = {|{ "age": 5 }["name"]|} in
  let result = setup input in
  result = Null

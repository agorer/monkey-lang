module Repl = Interpreter.Repl
module Environment = Interpreter.Environment
                
let () =
  Printf.fprintf Out_channel.stdout "REPL for the Monkey programming language\n%!";
  let env = Environment.make () in
  Repl.start In_channel.stdin Out_channel.stdout env
    
    

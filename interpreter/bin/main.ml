module Repl = Interpreter.Repl
                
let () =
  Printf.fprintf Out_channel.stdout "REPL for the Monkey programming language\n%!";
  Repl.start In_channel.stdin Out_channel.stdout
    
    

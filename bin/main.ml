include Interpreter

let () =
  print_endline "HEllo! This is the Monkey programming language!\n";
  print_endline "Feel free to type in commands\n";
  match Array.length Sys.argv with
    | _ -> (try while true do Interpreter.run () done with End_of_file -> ())
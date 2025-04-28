open Interpreter

include Object
include Ast
include Env
include Builtin
include Parser

module To_test = struct
  let evals strlst = let rec reval = function
        | [] -> []
        | h::t -> let (ps, prg) = Parser.parse_program (Parser.new_parser (Lexer.new_lexer h)) []
        in let obj = Evaluator.eval_program ps.errors prg.statements (Env.new_env ())
        in obj::(reval t)
    in reval strlst
end

let obj_test = Alcotest.testable Object.pp Object.equal

let test_fibonacci () = Alcotest.(check (list obj_test))
  "same objs"
  [
    Object.Integer 55;
  ]
  (To_test.evals [
    "let fib = fn (a) {
      if (a == 0) {return 0}
      else {
        if (a == 1) { return 1 }
        else { return fib (a-1) + fib (a-2) }
      }
    };
    fib (10)";
  ])

  let () =
  let open Alcotest in
  run "evaluator" [
      "eval", [
          test_case "eval Fibonacci" `Slow test_fibonacci;
      ];
  ]
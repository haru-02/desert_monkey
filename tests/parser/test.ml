open Interpreter

include Parser
include Ast
include Lexer

module To_test = struct
  let ast lex = let (_, prg) = Parser.parse_program (Parser.new_parser lex) [] in prg.statements
end

let ast_test = Alcotest.testable Ast.pp Ast.eq
let test_statements () = Alcotest.(check (list ast_test))
  "same ast"
  [
    Ast.ReturnStatement {value = Ast.Identifier "haru"};

    Ast.LetStatement {
      name = Ast.Identifier "x"; 
      value = Ast.InfixExpression {
        op = "+";
        left = Ast.PrefixExpression {op = "-"; right = Ast.IntegerLiteral 1;};
        right = Ast.InfixExpression {
          op = "*";
          left = Ast.IntegerLiteral 1;
          right = Ast.IntegerLiteral 5;
        };
      }
    };

    Ast.ExpressionStatement {
      expression = Ast.InfixExpression {
        op = "+";
        left = Ast.PrefixExpression {op = "!"; right = Ast.Identifier "tester";};
        right = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 12; right = Ast.BooleanLiteral true ;}; 
      }
    };

    Ast.ExpressionStatement {expression = Ast.BooleanLiteral false};

    Ast.ExpressionStatement {
      expression = Ast.IfExpression {
        cond = Ast.InfixExpression {
          op = "==";
          left = Ast.InfixExpression {op = "*"; left = Ast.IntegerLiteral 2; right = Ast.IntegerLiteral 3};
          right = Ast.Identifier "tester";
        };
        cons = Ast.BlockStatement {
          statements = [
            Ast.LetStatement {name = Ast.Identifier "tester"; value = Ast.IntegerLiteral 3};
            Ast.ExpressionStatement {
              expression = Ast.InfixExpression {op = "=="; left = Ast.Identifier "dummy"; right = Ast.Identifier "tester";}
            };
          ]
        };
        alt = Some (Ast.BlockStatement {
          statements = [
            Ast.ExpressionStatement {expression = Ast.Identifier "testerTwo"};
          ]
        });
      }
    };
    
    Ast.ExpressionStatement {
      expression = Ast.ArrayLiteral {
        elms = [
          Ast.IntegerLiteral 1;
          Ast.IntegerLiteral 3;
          Ast.FunctionLiteral {
            prms = [
              Ast.Identifier "a";
              Ast.Identifier "b";
            ];
            body = Ast.BlockStatement {
              statements = [
                Ast.ExpressionStatement {
                  expression = Ast.InfixExpression {
                    op = "+";
                    left = Ast.Identifier "a";
                    right = Ast.Identifier "b";
                  }
                };
              ]
            }
          }
        ]
      }
    };
    
    Ast.ExpressionStatement {
      expression = Ast.IndexExpression {left = Ast.Identifier "Array"; index = Ast.Identifier "index"}
    };
  ]

  (Lexer.new_lexer "return haru;
    let x = -1 + 1 * 5;
    !tester + 12 * true;
    false;
    if (2 * 3 == tester) { let tester = 3; dummy == tester;} else {testerTwo};
    [1, 3, fn (a, b) {a+b}];
    Array[index];
  " |> To_test.ast)

let () =
  let open Alcotest in
  run "Lexer" [
      "nextoken", [
          test_case "parse statements" `Slow test_statements;
      ]
    ]

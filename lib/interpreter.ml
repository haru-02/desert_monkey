module Lexer = Lexer.Lexer
module Token = Token.Token
module Ast = Ast.Ast
module Parser = Parser.Parser
module Evaluator = Evaluator.Evaluator
module Object = Object.Object
module Env = Env.Env
module Builtin = Builtin.Builtin
module Macro = Macro.Macro

let prompt = ">>"
let monkey = {|            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----''|}


let run () = print_string prompt;
  let input = read_line ()
  in let (ps, prg) = Parser.parse_program (Parser.new_parser (Lexer.new_lexer input)) []
  in let p, e = (Macro.define_macros prg.statements (Env.get_env))
  in let pr = Macro.expand_macros p e
  in print_endline (match (Evaluator.eval_program ps.errors pr Env.get_env) with
    | Object.Empty -> ""
    | Object.Error i -> monkey ^ "\nWoops! We ran into some monkey business here!\n Error: " ^ i
    | obj -> Object.obj_to_string obj)
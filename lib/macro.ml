module Macro = struct
  include Ast
  include Env
  include Object
  include Evaluator
  
    let is_macro node = match node with
    | Ast.LetStatement i -> (match i.value with
      | Ast.MacroLiteral _ -> true
      | _ -> false)
    |_ -> false
    let define_macros prg env = let rec add_macro pg ev: Ast.statement list * Env.env = match pg with
    | [] -> (pg, ev)
    | h::t -> (match h with
      | Ast.LetStatement {
          name = Ast.Identifier i; value = Ast.MacroLiteral m
        } ->let _ = Env.set ev i (Object.Macro {prms = m.prms; body = m.body;}) in add_macro t ev
      | _ -> let p, e = add_macro t ev in h::p, e)
    in add_macro prg env
  
    let expand_macros prg env = let modifier nd = match nd with
      | Ast.CallExpression {fn = Ast.Identifier idt; args} -> (match Env.get env idt with
        | Some (Object.Macro m) -> let nenv = Env.extend_macro_env m.prms args env
          in (match Evaluator.eval_statement m.body nenv with
          | (Object.Quote q, _) -> q
          | _ -> nd)
        | _ -> nd)
      | _ -> nd
    in let rec expandMacro = function
      | [] -> []
      | h::t -> (Ast.modify_statement modifier h)::(expandMacro t)
    in expandMacro prg

end
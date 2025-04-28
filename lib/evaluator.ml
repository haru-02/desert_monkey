module Evaluator = struct
  include Object
  include Ast
  include Builtin
  include Parser
  include Env

  let is_truthy = function
    | Object.Boolean boolean -> boolean
    | Object.Null -> false
    | _ -> true

  let convert = function
    | Object.Integer i -> Ast.IntegerLiteral i
    | Object.String s -> Ast.StringLiteral s
    | Object.Boolean b -> Ast.BooleanLiteral b
    | Object.Function f -> Ast.FunctionLiteral {prms = f.prms; body = f.body;}
    | Object.Quote i -> i
    | i -> raise(Failure ("Cannot convert " ^ (Object.obj_to_string i)))
  
  let eval_prefix_expression op right = match op with
    | "!" -> Object.Boolean (Bool.not (is_truthy right))
    | "-" -> (match right with
      | Object.Integer i -> Object.Integer (-i)
      | o -> Object.Error ("Unknown operator: " ^ op ^ " " ^ (Object.obj_to_string o)))
    | _ -> Object.Error ("Unknown operator: " ^ op ^ " " ^ (Object.obj_to_string right))
  
  let eval_integer_infix_expression op left right = match op with
    | "+" -> Object.Integer (left + right)
    | "-" -> Object.Integer (left - right)
    | "*" -> Object.Integer (left * right)
    | "/" -> Object.Integer (left / right)
    | "%" -> Object.Integer (left mod right)
    | "==" -> Object.Boolean (left = right)
    | "!=" -> Object.Boolean (left <> right)
    | "<" -> Object.Boolean (left < right)
    | "<=" -> Object.Boolean (left <= right)
    | ">" -> Object.Boolean (left > right)
    | ">=" -> Object.Boolean (left >= right)
    | _ -> Object.Error ("Unknown operator: " ^ op ^ " " ^ string_of_int(left) ^ " " ^ string_of_int(right))

  let eval_string_expression op left right = match op with
    | "+" -> Object.String (left ^ right)
    | "==" -> Object.Boolean (left = right)
    | "!=" -> Object.Boolean (left <> right)
    | _ -> Object.Error ("Unknown operator: " ^ op ^ " " ^ left ^ " " ^ right)

  let eval_infix_expression op left right = match (left, right) with
    | Object.Integer l, Object.Integer r -> eval_integer_infix_expression op l r
    | Object.String l, Object.String r -> eval_string_expression op l r
    | Object.Array l, Object.Array r -> (match op with
      | "+" -> Object.Array (l @ r)
      | _ -> Object.Error ("Unknown operator on array : " ^ op ^ " " ^ (Object.obj_to_string left) ^ " " ^ (Object.obj_to_string right)))
    | l, r -> if Object.type_equals l r
      then (match op with
        | "==" -> Object.Boolean (l = r)
        | "!=" -> Object.Boolean (l <> r)
        | _ -> Object.Error ("Unknown operator: " ^ op ^ " " ^ (Object.obj_to_string left) ^ " " ^ (Object.obj_to_string right)))
      else Object.Error ("Type mismatch: " ^ op ^ " " ^ (Object.obj_to_string left) ^ " " ^ (Object.obj_to_string right))
  
  let eval_identifier idt env = match Env.get env idt with
    | Some var -> (var, env)
    | None -> (match Builtin.get idt with
      | Some btin -> (Object.BuiltIn btin, env)
      |None -> (Object.Error ("Identifier not found: " ^ idt), env))
      
  let rec eval_expression exp env = match exp with
    | Ast.IntegerLiteral i -> (Object.Integer i, env)
    | Ast.StringLiteral s -> (Object.String s, env)
    | Ast.BooleanLiteral b -> (Object.Boolean b, env)
    | Ast.Identifier i -> eval_identifier i env
    | Ast.PrefixExpression {op; right;} -> let (r,ev) = eval_expression right env in (eval_prefix_expression op r, ev)
    | Ast.InfixExpression {op; left; right;} -> let (l, ev) = eval_expression left env
      in let (r, ev) = eval_expression right ev
      in (eval_infix_expression op l r, ev)
    | Ast.IfExpression e -> eval_if_expression e.cond e.cons e.alt env
    | Ast.FunctionLiteral {prms; body;} -> (Object.Function {prms = prms; body = body;}, env)
    | Ast.CallExpression {fn; args} -> (match fn with
      | Ast.Identifier i when i = "quote" -> (match args with
        | h::[] -> ((quote h env), env)
        | _ -> (Object.Error "a quote expression must have one argument only", env))
      | _-> (match eval_expression fn env with
        | Object.Error e, ev -> (Object.Error e, ev)
        | Object.BuiltIn b, ev -> (match eval_expressions args ev with
          | [Object.Error i], e -> (Object.Error i, e)
          | args, e -> match b args with
            | Some v -> (v, e)
            | None -> (Object.Error "function got called with wrong params", e))
        | Object.Function f, ev -> (match eval_expressions args ev with
          | [Object.Error i], e -> (Object.Error i, e)
          | args, e -> (match f.body with
            | Ast.BlockStatement stm -> (apply_function f.prms stm.statements args e, e)
            | bd -> (Object.Error ("expected block statements got " ^ Ast.stm_to_string bd), e)))
        | exp, ev -> (Object.Error (Object.obj_to_string exp), ev)))
    | Ast.ArrayLiteral a -> (match eval_expressions a.elms env with
      | [Object.Error i], e -> (Object.Error i, e)
      | args, e -> (Object.Array args, e))
    | Ast.IndexExpression i -> (match eval_expression i.left env with
      | Object.Array arr, ev -> (match eval_expression i.index ev with
        | Object.Integer it, e -> (match List.nth_opt arr it with
          | Some obj -> (obj, e)
          | None -> (Object.Error "index out of range", e))
        | obj, e -> (Object.Error ("index operator expects an integer, got " ^ (Object.obj_to_string obj)), e))
      | obj, e -> (Object.Error ("index operator expects an array, got " ^ (Object.obj_to_string obj)), e))
    | _ -> (Object.Error ("Unknown expression: " ^ (Ast.exp_to_string exp)), env)

    and eval_if_expression cond cons alt env = match (eval_expression cond env) with
      | Object.Error e, ev -> (Object.Error e, ev)
      | obj, ev -> if is_truthy obj
        then match cons with
          | Ast.BlockStatement b -> eval_block_statement b.statements ev
          | _ -> (Object.Error "expected a block statement", ev)
        else (match alt with
          | Some (Ast.BlockStatement stm) -> eval_block_statement stm.statements ev
          | Some _ -> (Object.Error "expected a block statement", ev)
          | None -> (Object.Null, ev))
    
    and eval_expressions exps env = let rec r exps ev = match exps with
      | [] -> ([], ev)
      | h::t -> match eval_expression h ev with
        | Object.Error e, _ -> raise(Failure e)
        | obj, e -> let (objs, envr) = r t e in (obj::objs, envr)
    in try r exps env with
      | Failure e -> [Object.Error e], env
      | _ -> ([Object.Error "Unknown expression error"], env)
    
    and apply_function prms body args env = let nenv = Env.extend_function_env prms args env in
    match eval_block_statement body nenv with
      | Object.Return i, _ -> i
      | obj, _ -> obj

    and eval_statement stm env = match stm with
      | Ast.ExpressionStatement i -> eval_expression i.expression env
      | Ast.BlockStatement i -> eval_block_statement i.statements env
      | Ast.ReturnStatement i -> eval_return_statement i.value env
      | Ast.LetStatement i -> eval_let_statement i.name i.value env
      | _ -> (Object.Error ("Unknown statement: " ^ (Ast.stm_to_string stm)), env)

    and eval_block_statement (blstm: Ast.statement list) env = let rec rebs slist ev = match slist with
      | [] -> (Object.Empty, ev)
      | h::[] -> eval_statement h ev
      | h::t -> match eval_statement h ev with
        | Object.Error i, e -> (Object.Error i, e)
        | Object.Return i, e -> (i, e)
        | _, e -> rebs t e
    in match rebs blstm env with
        | ob, e -> (ob, e)
    
    and eval_return_statement value env = (match eval_expression value env with
      | Object.Error e, env -> (Object.Error e, env)
      | obj, ev -> (Object.Return obj, ev))
    
    and eval_let_statement idt value env = match idt with
      | Ast.Identifier i -> (match eval_expression value env with
        | Object.Error e, ev -> (Object.Error e, ev)
        | obj, ev -> (Env.set ev i obj, ev))
      | node -> (Object.Error ("Expected identifier, got " ^ (Ast.exp_to_string node)), env)
    
    and eval_unquote_calls node env =
      let modifier expr =
        match expr with
        | Ast.CallExpression { fn = Ast.Identifier "unquote"; args = [arg] } -> let obj, _ = eval_expression arg env in convert obj
        | _ -> expr
      in Ast.modify_expression modifier node
   
    and quote node env = let qt = (eval_unquote_calls node env) in Object.Quote qt
  
    let eval_program perrors program env = if perrors = []
    then let rec revs slist ev = match slist with
      | [] -> (Object.Empty, ev)
      | h::[] -> (match eval_statement h ev with
        | Object.Return i, e -> (i, e)
        | obj, e -> (obj, e))
      | h::t -> match eval_statement h ev with
        | Object.Return i, e -> (i, e)
        | Object.Error i, e -> (Object.Error i, e)
        | _, e -> revs t e
    in match revs program env with
    | ob, _ -> ob
    else Object.Error (Parser.errors_to_string perrors)
         
end
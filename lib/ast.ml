module Ast = struct
  include Token
  
  type node =
    | Expression of expression
    | Statement of statement

  and expression =
    | IntegerLiteral of int
    | StringLiteral of string
    | Identifier of string
    | BooleanLiteral of bool
    | PrefixExpression of {op: string; right: expression}
    | InfixExpression of {op: string; left: expression; right: expression;}
    | IfExpression of {cond: expression; cons: statement; alt: statement option;}
    | FunctionLiteral of {prms: expression list; body: statement}
    | ArrayLiteral of {elms: expression list;}
    | CallExpression of {fn: expression; args: expression list;}
    | IndexExpression of {left: expression; index: expression}
    | MacroLiteral of {prms: expression list; body: statement}
  
  and statement =
    | LetStatement of {name: expression; value: expression}
    | ReturnStatement of {value: expression}
    | ExpressionStatement of {expression: expression}
    | BlockStatement of {statements: statement list}
    | IfStatement of {cond: expression; cons: statement; alt: statement option}
    | WhileStatement of {cond: expression; body: statement}
    | ForStatement of {init: statement; cond: expression; update: statement; body: statement}
    | BreakStatement
    | ContinueStatement
    (* | ImportStatement of {path: string}
    | ExportStatement of {path: string} *)
    (* TODO this feature is future me problem *)

  type program = {
    statements: statement list;
  }
  
  let rec exp_to_string = function
    | IntegerLiteral i -> "(INT " ^ string_of_int i ^ ") "
    | StringLiteral s -> "(STR " ^ s ^ ") "
    | Identifier i -> "(IDT " ^ i ^ ") "
    | BooleanLiteral i -> "(BOOL " ^ string_of_bool i ^ ") "
    | PrefixExpression i -> "(PREFIX {op: " ^ i.op ^ " right:{" ^ exp_to_string i.right ^ "}}) "
    | InfixExpression i -> "(INFIX {op: " ^ i.op ^ " left:{" ^ exp_to_string i.left ^ "}"
      ^ " right:{" ^ exp_to_string i.right ^ "}}) "
      | IfExpression i -> let alt = match i.alt with
      | Some i -> stm_to_string i
      | None -> "" in "(IF {cond: " ^ exp_to_string i.cond ^
      " cons: " ^ stm_to_string i.cons
      ^ alt ^ "})"
    | FunctionLiteral i -> "(FN {prms: " ^ exps_to_string i.prms ^ " body: " ^ stm_to_string i.body ^ "})"
    | ArrayLiteral i -> exps_to_string i.elms
    | CallExpression i -> "(CALLEXP {fn: " ^ exp_to_string i.fn ^ " args: " ^ exps_to_string i.args ^ "})"
    | IndexExpression i -> "(INDEXEXP {left: " ^ exp_to_string i.left ^ " index: " ^ exp_to_string i.index ^ "})"
    | MacroLiteral i -> "(MACRO {prms: " ^ exps_to_string i.prms ^ " body: " ^ stm_to_string i.body ^ "})" 
  
  and exps_to_string = function
    | [] -> ""
    | h::t -> "[ " ^ exp_to_string h ^ " ]" ^ (exps_to_string t)
  
  and stm_to_string = function
    | LetStatement i -> "LET:" ^ exp_to_string i.name ^ " " ^ exp_to_string i.value
    | ReturnStatement i -> "RET:" ^ exp_to_string i.value
    | ExpressionStatement i -> "EXP:" ^ exp_to_string i.expression
    | BlockStatement i -> let rec rstmts = function
      | [] -> ""
      | h::t -> "[ " ^ stm_to_string h ^ " ]" ^ rstmts t
    in "BLOCK:" ^ rstmts i.statements
    | _ -> "default" (* handle this default case TODO, done to remove the exhaustive case match warning*)
  
  let rec modify_expression (modifier:expression -> expression) node = 
    let nd = match node with
    | InfixExpression i -> InfixExpression {
        i with
        left = modify_expression modifier i.left;
        right = modify_expression modifier i.right;
      }

    | PrefixExpression i -> PrefixExpression {
        i with
        right = modify_expression modifier i.right;
      }
          
    | IfExpression i -> (match i.alt with
      | Some alt -> IfExpression {
          cond = modify_expression modifier i.cond;
          cons = modify_statement modifier i.cons;
          alt = Some (modify_statement modifier alt);
        }
      
      | None -> IfExpression {
          cond = modify_expression modifier i.cond;
          cons = modify_statement modifier i.cons;
          alt = None;
        })

    | FunctionLiteral i -> FunctionLiteral {
        prms = List.map (modify_expression modifier) i.prms;
        body = modify_statement modifier i.body;
      }

    | ArrayLiteral i -> ArrayLiteral {
        elms = List.map (modify_expression modifier) i.elms;
      }

    | IndexExpression i -> IndexExpression {
        left = modify_expression modifier i.left;
        index = modify_expression modifier i.index;
      }

    | CallExpression i -> (match i.fn with
      | Identifier idt when idt = "unquote" -> CallExpression i
      | _ -> CallExpression {
          fn = modify_expression modifier i.fn;
          args = List.map (modify_expression modifier) i.args;
        })

    | exp -> exp
    in modifier nd

  and modify_statement modifier stm = match stm with
  | LetStatement i -> LetStatement {
      name = modify_expression modifier i.name;
      value = modify_expression modifier i.value;
    }
  | ReturnStatement i -> ReturnStatement {
      value = modify_expression modifier i.value;
    }
  | ExpressionStatement i -> ExpressionStatement {
      expression = modify_expression modifier i.expression;
    }
  | BlockStatement i -> BlockStatement {
      statements = List.map (modify_statement modifier) i.statements;
    }
  | _ -> stm (* handle this default case TODO, done to remove the exhaustive case match warning*)


  let stms_to_string stmts = let rec rrc = function
  | [] -> ""
  | h::t -> (stm_to_string h) ^ " " ^ rrc t
  in rrc stmts

  let eq a b = a = b
  let pp ppf st = Fmt.pf ppf "Statement = %s" (stm_to_string st)
end
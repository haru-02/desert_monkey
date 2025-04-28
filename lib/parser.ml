module Parser = struct
  include Lexer
  include Token
  include Ast

  type parser = {
    lex : Lexer.lexer;
    current_token : Token.token;
    peek_token : Token.token;
    errors : string list;
  }

  let new_parser lex = let (tok,le) = Lexer.next_token lex in
  let (n_tok, _) = Lexer.next_token le in { 
    lex = le;
    current_token = tok;
    peek_token = n_tok;
    errors = [];
  }

  let next_token p =
    let (_, le) = Lexer.next_token p.lex in
    let (tok, _) = Lexer.next_token le in { 
      p with
      lex = le;
      current_token = p.peek_token;
      peek_token = tok;      
    }

  let prefix_precedence = 7
  let lowest_precedence = 1
  let precedence tok = match tok with
  | Token.EQ -> 2
  | Token.NOT_EQ -> 2
  | Token.LT -> 3
  | Token.GT -> 3
  | Token.PLUS -> 4
  | Token.MINUS -> 4
  | Token.DIV -> 5
  | Token.MUL -> 5
  | Token.LPAREN -> 6
  | Token.LBRACKET -> 8
  | _ -> lowest_precedence

  let rec errors_to_string errors = match errors with
  | [] -> ""
  | h::[] -> h
  | h::t -> h ^ ", " ^ (errors_to_string t)

  let error_parser p msg = ({
    p with
    errors = p.errors @ [" " ^ msg ^ " at " ^ Token.token_to_string p.current_token] (*TODO: can you make this line and col no later ?*)
  }, None)

  let peek_precedence p = precedence p.peek_token.t_type
  let parse_integer p = (p, Some (Ast.IntegerLiteral (int_of_string p.current_token.literal)))
  let parse_identifier p = (p, Some (Ast.Identifier p.current_token.literal))
  let parse_string p = (p, Some (Ast.StringLiteral p.current_token.literal))
  let parse_boolean p = (p, Some (Ast.BooleanLiteral (bool_of_string p.current_token.literal)))

  let parse_block_statement prs parse_statement =
    let rec parse_stmts prs acc =
      match acc with
      | Some slist -> (match prs.current_token.t_type with
      | Token.EOF -> error_parser prs "expected '}' at EOF"
      | Token.RBRACE -> (prs, Some slist)
      | _ -> match parse_statement prs with
        | (ps, Some stm) -> parse_stmts (next_token ps) (Some (slist@[stm]))
        | (ps, None) -> (ps, None))
    | None -> (prs, None)
    in match parse_stmts prs (Some []) with
    | (pr, Some slist) -> (pr, Some (Ast.BlockStatement {statements = slist;}))
    | (pr, None) -> (pr, None)
  
  let parse_if_expression prs parseExpression parseStatement =
    if prs.peek_token.t_type = Token.LPAREN
    then (match parseExpression (next_token prs |> next_token) lowest_precedence parseStatement with
    | (pr, Some exp) -> let pr = next_token pr in if pr.current_token.t_type = Token.RPAREN && pr.peek_token.t_type = Token.LBRACE
      then (match parse_block_statement (next_token pr |> next_token) parseStatement with
      | (par, Some ex) -> let ps = next_token par in if ps.current_token.t_type = Token.ELSE
          then if ps.peek_token.t_type = Token.LBRACE
            then (match parse_block_statement (next_token ps |> next_token) parseStatement with
            | (p, Some expr) -> (p, Some (Ast.IfExpression 
              { cond = exp; 
                cons = ex; 
                alt = Some expr}))
            | (p, None) -> (p, None))
          else error_parser ps "expected '{' after ELSE"
          else (par, Some (Ast.IfExpression 
          {cond = exp; 
          cons = ex; 
          alt = None;}))
      | (par, None) -> (par, None))
        else error_parser pr "expected ')' after condition"
    | (pr, None) -> (pr, None))
    else error_parser prs "expected '(' after IF"
  
  let parse_function_parameters prs = let rec rpfp par fps = (match fps with
    | Some plist -> if par.current_token.t_type = Token.IDENT
      then (match par.peek_token.t_type with
      | Token.RPAREN -> (match parse_identifier par with
        | (pr, Some exp) -> (next_token pr, Some (plist @ [exp]))
        | (pr, None) -> (pr, None))
      | Token.COMMA -> (match parse_identifier par with
        | (pr, Some exp) -> rpfp (next_token pr |> next_token) (Some (plist @ [exp]))
        | (pr, None) -> (pr, None))
      | _ -> error_parser (next_token par) "expected COMMA or ')'")    
      else error_parser par "expected identifier"
    | None -> (par, None))
    in if prs.peek_token.t_type = Token.RPAREN
      then (next_token prs, Some [])
      else rpfp (next_token prs) (Some [])
  
  let parse_function_structure prs parse_statement = 
    if prs.peek_token.t_type = Token.LPAREN then 
      match parse_function_parameters (next_token prs) with
        | (pr, Some plist) -> if pr.peek_token.t_type = Token.LBRACE
          then match parse_block_statement (next_token pr |> next_token) parse_statement with
            | (p, Some slist) -> (p, Some (plist, slist))
            | (p, None) -> (p, None)
          else error_parser prs "expected '{' after function body"
        | (pr, None) -> (pr, None)
    else error_parser prs "expected '(' after function"

  let parse_expression_list prs parse_expression parse_statement endt = let rec rpel par expl = (match expl with
    | Some elist -> (match parse_expression par lowest_precedence parse_statement with 
      | (pr, Some exp) -> (match pr.peek_token.t_type with
        | Token.COMMA -> rpel (next_token pr |> next_token) (Some (elist@[exp]))
        | tok -> if tok = endt
          then (next_token pr, Some (elist@[exp]))
          else error_parser pr "expected ','")
      | (pr, None) -> (pr, None))
    | None -> (par, None))
    in if prs.peek_token.t_type = endt
      then (next_token prs, Some [])
      else rpel (next_token prs) (Some [])
  
  let parse_array prs parse_expression parse_statement endt = match parse_expression_list prs parse_expression parse_statement endt with
    | (pr, Some elist) -> (pr, Some (Ast.ArrayLiteral {elms = elist}))
    | (pr, None) -> (pr, None)
  
  let parse_call_expression prs lexp parse_expression parse_statement = match parse_expression_list prs parse_expression parse_statement Token.RPAREN with
      | (pr, Some elist) -> (pr, Some (Ast.CallExpression { fn = lexp; args = elist;}))
      | (pr, None) -> (pr, None)
  
  let parse_index_expression prs lexp parse_expression parse_statement = match parse_expression (next_token prs) lowest_precedence parse_statement with
    | (pr, Some exp) -> (next_token pr, Some (Ast.IndexExpression {left = lexp; index = exp;}))
    | (pr, None) -> (pr, None)
  
  let parse_prefix_expression par parse_expression parse_statement = (match par.current_token with
    | {literal = _; t_type = Token.INT; _} -> parse_integer par
    | {literal = _; t_type = Token.IDENT; _} -> parse_identifier par
    | {literal = _; t_type = Token.STRING; _} -> parse_string par
    | {literal = _; t_type = Token.TRUE; _} -> parse_boolean par
    | {literal = _; t_type = Token.FALSE; _} -> parse_boolean par
    | {literal = _; t_type = Token.IF; _} -> parse_if_expression par parse_expression parse_statement
    | {literal = _; t_type = Token.FUNCTION; _} -> (match parse_function_structure par parse_statement with 
      | (p, Some (prms, body)) -> (p, Some (Ast.FunctionLiteral {prms = prms; body = body;}))
      | (p, None) -> (p, None))
    | {literal = _ ; t_type = Token.LBRACKET; _} -> parse_array par parse_expression parse_statement Token.RBRACKET
    | {literal = _; t_type = Token.LPAREN; _} ->  (match (parse_expression (next_token par) lowest_precedence parse_statement) with
      | (pr, Some exp) -> if pr.peek_token.t_type = Token.RPAREN
        then (next_token pr, Some exp)
        else error_parser pr "expected ')'"
      | (pr, None) -> (pr, None))
    | {literal; t_type = Token.BANG; _} -> (match parse_expression (next_token par) prefix_precedence parse_statement with
      | (pr, Some exp) -> (pr, Some (Ast.PrefixExpression {op = literal; right = exp}))
      | (pr, None) -> (pr, None))
    | {literal; t_type = Token.MINUS; _} -> (match parse_expression (next_token par) prefix_precedence parse_statement with
      | (pr, Some exp) -> (pr, Some (Ast.PrefixExpression {op = literal; right = exp}))
      | (pr, None) -> (pr, None))
    | {literal = _; t_type = Token.MACRO; _} -> (match parse_function_structure par parse_statement with
      | (p, Some (prms, body)) -> (p, Some (Ast.MacroLiteral {prms = prms; body = body;}))
      | (p, None) -> (p, None))
    | _ -> error_parser par "no matching prefix parse")

  let parse_infix_expression par lexp parseExpression parseStatement = match par.current_token with
  | {literal; t_type = Token.DIV; _}
  | {literal; t_type = Token.MUL; _}
  | {literal; t_type = Token.EQ; _}
  | {literal; t_type = Token.NOT_EQ; _}
  | {literal; t_type = Token.LT; _}
  | {literal; t_type = Token.GT; _}
  | {literal; t_type = Token.MINUS; _}
  | {literal; t_type = Token.PLUS; _} -> (match parseExpression (next_token par) (precedence par.current_token.t_type) parseStatement with
    | (pr, Some exp) -> (pr, Some (Ast.InfixExpression {op = literal; left = lexp; right = exp;}))
    | (pr, None) -> (pr, None))
  | {literal = _; t_type = Token.LPAREN; _} -> parse_call_expression par lexp parseExpression parseStatement
  | {literal = _; t_type = Token.LBRACKET; _} -> parse_index_expression par lexp parseExpression parseStatement
  | _ -> error_parser par "no matching infix parse"
  
  let rec parse_expression prs pcd parse_statement = match (parse_prefix_expression prs parse_expression parse_statement) with
  | (ps, Some le) ->
    let rec r_parse_infix pr prcd lexp = (if prcd < (peek_precedence pr)
      then (match (parse_infix_expression (next_token pr) lexp parse_expression parse_statement) with
      | (pars, Some re) -> r_parse_infix pars prcd re
      | (pars, None) -> (pars, None))
      else (pr, Some lexp))
    in r_parse_infix ps pcd le
  | (ps, None) -> (ps, None)

  let parse_expression_statement prs parse_statement = match parse_expression prs lowest_precedence parse_statement with
  | (ps, Some exp) -> (let p = if Token.isSemicolon ps.peek_token then next_token ps else ps
        in (p, Some (Ast.ExpressionStatement {
          expression = exp;
        })))
  | (ps, None) -> (ps, None)
  
  let parse_let_statement prs parse_statement = match prs.current_token with
  | {literal; t_type = Token.IDENT; _} -> let pr = next_token prs in (match pr.current_token with
    | {literal = _; t_type = Token.ASSIGN; _} -> (match parse_expression (next_token pr) lowest_precedence parse_statement with
      | (p, Some e) -> let sp = if Token.isSemicolon p.peek_token then next_token p else p
          in (sp, Some (Ast.LetStatement {
              name = Ast.Identifier literal;
              value = e
          }))
      | (p, None) -> (p, None))
    | _  -> error_parser pr "expected ASSIGN")
  | _ -> error_parser prs "expectd IDENT"

  let parse_return_statement prs parse_statement = match parse_expression prs lowest_precedence parse_statement with
  | (ps, Some exp) -> (let p = if Token.isSemicolon ps.peek_token then next_token ps else ps
        in (p, Some (Ast.ReturnStatement {
          value = exp;
        })))
  | (ps, None) -> (ps, None)

  let rec parse_statement prs = match prs.current_token with
    | {literal = _; t_type = Token.LET; _} -> parse_let_statement (next_token prs) parse_statement
    | {literal = _; t_type = Token.RETURN; _} -> parse_return_statement (next_token prs) parse_statement
    | _ -> parse_expression_statement prs parse_statement

  let parse_program prs lst : parser * Ast.program = let rec rpp prs prg = match prs.current_token with
  | {literal = _; t_type = Token.EOF; line= _; col = _} -> (prs, prg)
  | _ -> match parse_statement prs with
    | (ps, Some stm) -> rpp (next_token ps) (prg@[stm])
    | (ps, None) -> (ps, prg)
  in match rpp prs lst with
  | (ps, prg) -> (ps, {statements = prg;})

  let eq prsa prsb = Token.eq prsa.current_token prsb.current_token && Token.eq prsa.peek_token prsb.peek_token

  let pp ppf prs = Fmt.pf ppf "parser = { %s }" ("current_token:" ^ Token.token_to_string prs.current_token ^ " peek_token:" ^ Token.token_to_string prs.peek_token)
end
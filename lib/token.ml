module Token = struct
  include Fmt
  
  type token_type = 
  (* special tokens *)
  | ILLEGAL
  | EOF
  (* indetifiers and literals *)
  | STRING
  | INT
  | IDENT
  | FLOAT
  | BOOL
  (* comments *)
  | COMMENT
  (* operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | DIV
  | BANG
  | DOT
  | MUL
  | MOD
  | EQ
  | NOT_EQ
  | LT
  | GT
  (* delimiters *)
  | COMMA
  | SEMICOLON
  | COLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  (* keywords *)
  | FUNCTION
  | LET
  | TRUE
  | FALSE
  | IF
  | ELSE
  | RETURN
  | WHILE
  | FOR
  | BREAK
  | CONTINUE
  | NIL
  | AND
  | OR
  | NOT
  | IN
  | THIS
  | MACRO
  
  type token = {
    literal: string;
    t_type: token_type;
    line: int;
    col: int;
  }

  let token_to_string tok = match tok.t_type with
  (* special tokens *)
  | ILLEGAL -> "ILLEGAL :" ^ tok.literal
  | EOF -> "END OF FILE"
  (* indentifiers and literals *)
  | STRING -> "STRING :" ^ tok.literal
  | INT -> "INTEGER :" ^ tok.literal
  | IDENT -> "IDENTIFIER :" ^ tok.literal
  | FLOAT -> "FLOATING_POINT :" ^ tok.literal
  | BOOL -> "BOOLEAN :" ^ tok.literal
  (* comments *)
  | COMMENT -> "COMMENT"
  (* operators *)
  | ASSIGN -> "ASSIGN"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | DIV -> "DIV"
  | MUL -> "MUL"
  | MOD -> "MODULO"
  | EQ -> "EQ"
  | BANG -> "BANG"
  | DOT -> "DOT"
  | NOT_EQ -> "NOT_EQ"
  | LT -> "LT"
  | GT -> "GT"
  (* delimiters *)
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | COLON -> "COLON"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  (* keywords *)
  | FUNCTION -> "FUNCTION"
  | LET -> "LET"
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | WHILE -> "WHILE"
  | FOR -> "FOR"
  | BREAK -> "BREAK"
  | CONTINUE -> "CONTINUE"
  | NIL -> "NIL"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | IN -> "IN"
  | THIS -> "THIS"
  | MACRO -> "MACRO"
  
  (* 
    This is a list of keywords that will be recognised, 
    the rest will be classified as identifiers.
  *)
  let keywords = function
  | "fn" -> FUNCTION
  | "let" -> LET
  | "if" -> IF
  | "else" -> ELSE
  | "true" -> TRUE
  | "false" -> FALSE
  | "return" -> RETURN
  | "while" -> WHILE
  | "for" -> FOR
  | "break" -> BREAK
  | "continue" -> CONTINUE
  | "nil" -> NIL
  | "and" -> AND
  | "or" -> OR
  | "not" -> NOT
  | "in" -> IN
  | "this" -> THIS
  | "macro" -> MACRO
  | _ -> IDENT

  (* check if the given token is a semicolon *)
  let isSemicolon tok = tok.t_type = SEMICOLON
  let new_token ?(line:int = 0) ?(col:int = 0) t_type literal = {t_type; literal; line; col}
  let eq a b = a.t_type = b.t_type && a.literal = b.literal
  let pp ppf tk = Fmt.pf ppf "Token =%s" (token_to_string tk)

end
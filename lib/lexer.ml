module Lexer = struct
  include Token
  let null_byte = '\x00'
  
  type lexer = {  
    input: string;
    position: int;
    read_position: int;
    ch: char;
    line: int;
    col: int;
    }
  let mk (lex:lexer) t lit = Token.new_token ~line:lex.line ~col:lex.col t lit

  let is_letter c = 'a' <= c && c <= 'z' || 'A' <= c  && c <= 'Z' || c == '_'
  
  let is_digit c = '0' <= c && c <= '9'

  let is_alphanum c = is_letter c || is_digit c

  (* Read the next character from the input *)
  (* If we reach the end of the input, return null_byte *)
  (* This is used to check if we are at the end of the input *)
  (* and to read the next character from the input *)
  let rc lex = 
    if lex.read_position >= String.length(lex.input)
      then null_byte
  else
    String.get lex.input lex.read_position

  let read_char lex =
    let new_line, new_col =
      if lex.ch = '\n' then
        (lex.line + 1, 1)
      else
        (lex.line, lex.col+1)
    in
    { lex with 
      position = lex.read_position; 
      read_position = lex.read_position+1; 
      ch = rc lex;
      line = new_line;
      col = new_col;
    }
  
  let new_lexer input = read_char {
    input = input;
    position = 0;
    read_position = 0;
    ch = null_byte;
    line = 0;
    col = 0;
  }

  let rec skip_white_space lex = match lex.ch with
  | ' '
  | '\n'
  | '\t'
  | '\b'
  | '\r' -> skip_white_space(read_char lex)
  | _ -> lex


  let read lex f = let le = f lex in 
  (le, String.sub le.input lex.position (le.position - lex.position))
  
  let read_ident lex = 
    let rec advance le =
      match le.ch with
      | c when is_letter c -> advance (read_char le)
      | _ -> le
    in
    let final_lex = advance lex in
    let literal = String.sub lex.input lex.position (final_lex.position - lex.position) in
    let token_type = Token.keywords literal in
    let tok = Token.{ t_type = token_type; literal; line = lex.line; col = lex.col } in
    (tok, final_lex)
  
  let read_num lex =
    let rec advance le =
      match le.ch with
      | c when is_digit c -> advance (read_char le)
      | _ -> le
    in
    let final_lex = advance lex in
    let literal = String.sub lex.input lex.position (final_lex.position - lex.position) in
    let tok = Token.{ t_type = INT; literal; line = lex.line; col = lex.col } in
    (tok, final_lex)
  
  let read_string lex =
    let rec advance le =
      match le.ch with
      | '\x00'
      | '"' -> le
      | _ -> advance (read_char le) in
              let (le, str) = read lex advance in (read_char le, str)

  (* Read the next token from the input *)
  let next_token lex =
    let l = skip_white_space lex in
    match l.ch with
    | '=' -> if rc l = '=' then (mk l EQ "==", read_char (read_char l))
             else (mk l ASSIGN "=", read_char l)
    | '+' -> (mk l PLUS "+", read_char l)
    | '-' -> (mk l MINUS "-", read_char l)
    | '/' -> (mk l DIV "/", read_char l)
    | '*' -> (mk l MUL "*", read_char l)
    | '%' -> (mk l MOD "%", read_char l)
    | '!' -> if rc l = '=' then (mk l NOT_EQ "!=", read_char (read_char l))
             else (mk l BANG "!", read_char l)
    | '>' -> (mk l GT ">", read_char l)
    | '<' -> (mk l LT "<", read_char l)
    | ',' -> (mk l COMMA ",", read_char l)
    | ';' -> (mk l SEMICOLON ";", read_char l)
    | ':' -> (mk l COLON ":", read_char l)
    | ']' -> (mk l RBRACKET "]", read_char l)
    | '[' -> (mk l LBRACKET "[", read_char l)
    | ')' -> (mk l RPAREN ")", read_char l)
    | '(' -> (mk l LPAREN "(", read_char l)
    | '}' -> (mk l RBRACE "}", read_char l)
    | '{' -> (mk l LBRACE "{", read_char l)
    | '#' -> (mk l COMMENT "#", read_char l)
    | '"' -> let (l, str) = read_string(read_char l) in (mk l STRING str, l)
    | c when is_letter c -> read_ident l
    | c when is_digit c -> read_num l
    | '\x00' -> (mk l EOF "", l)
    | _ -> (mk l ILLEGAL (String.make 1 l.ch), read_char l)  

  let rec tokenize_all lex =
    let (tok, next_lex) = next_token lex in
    if tok.t_type = EOF then [tok]
    else tok :: tokenize_all next_lex  
end
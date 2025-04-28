open Interpreter
open Lexer
open Token

module To_test = struct
  let lex str =
    let rec g_tokens lx ls =
      let (tok, lx') = next_token lx in
      if tok.t_type = Token.EOF then ls @ [tok]
      else g_tokens lx' (ls @ [tok])
    in
    g_tokens (new_lexer str) []
end

let token_test = Alcotest.testable Token.pp Token.eq

let test_same_tok () =
  Alcotest.(check (list token_test))
    "same token"
    [
      Token.new_token Token.ASSIGN "=";
      Token.new_token Token.PLUS "+";
      Token.new_token Token.MINUS "-";
      Token.new_token Token.DIV "/";
      Token.new_token Token.MUL "*";
      Token.new_token Token.BANG "!";
      Token.new_token Token.COMMA ",";
      Token.new_token Token.SEMICOLON ";";
      Token.new_token Token.COLON ":";
      Token.new_token Token.LPAREN "(";
      Token.new_token Token.RPAREN ")";
      Token.new_token Token.LBRACE "{";
      Token.new_token Token.RBRACE "}";
      Token.new_token Token.LBRACKET "[";
      Token.new_token Token.RBRACKET "]";
      Token.new_token Token.BANG "!";
      Token.new_token Token.EQ "==";
      Token.new_token Token.ASSIGN "=";
      Token.new_token Token.ASSIGN "=";
      Token.new_token Token.BANG "!";
      Token.new_token Token.BANG "!";
      Token.new_token Token.NOT_EQ "!=";
      Token.new_token Token.STRING "haru-02 desmonky";
      Token.new_token Token.IDENT "aeiou";
      Token.new_token Token.INT "1234";
      Token.new_token Token.FUNCTION "fn";
      Token.new_token Token.ILLEGAL "~";
      Token.new_token Token.COMMENT "#";
      Token.new_token Token.LET "let";
      Token.new_token Token.IF "if";
      Token.new_token Token.ELSE "else";
      Token.new_token Token.TRUE "true";
      Token.new_token Token.FALSE "false";
      Token.new_token Token.RETURN "return";
      Token.new_token Token.EOF ""
    ]
    (To_test.lex "= + - /*!,;:(){}[] ! === =!!!=
      \"haru-02 desmonky\"aeiou1234fn~#let if else
    true false return")

let test_same_tok2 () =
  Alcotest.(check (list token_test))
    "same token"
    [
      Token.new_token Token.LET "let";
      Token.new_token Token.IDENT "a";
      Token.new_token Token.ASSIGN "=";
      Token.new_token Token.IDENT "b";
      Token.new_token Token.FUNCTION "fn";
      Token.new_token Token.LPAREN "(";
      Token.new_token Token.IDENT "a";
      Token.new_token Token.COMMA ",";
      Token.new_token Token.IDENT "b";
      Token.new_token Token.RPAREN ")";
      Token.new_token Token.LBRACE "{";
      Token.new_token Token.IDENT "a";
      Token.new_token Token.PLUS "+";
      Token.new_token Token.IDENT "b";
      Token.new_token Token.RBRACE "}";
      Token.new_token Token.INT "1";
      Token.new_token Token.LT "<";
      Token.new_token Token.INT "2";
      Token.new_token Token.EQ "==";
      Token.new_token Token.TRUE "true";
      Token.new_token Token.INT "1";
      Token.new_token Token.GT ">";
      Token.new_token Token.INT "2";
      Token.new_token Token.EQ "==";
      Token.new_token Token.FALSE "false";
      Token.new_token Token.MACRO "macro";
      Token.new_token Token.LPAREN "(";
      Token.new_token Token.IDENT "a";
      Token.new_token Token.COMMA ",";
      Token.new_token Token.IDENT "b";
      Token.new_token Token.RPAREN ")";
      Token.new_token Token.LBRACE "{";
      Token.new_token Token.IDENT "a";
      Token.new_token Token.MINUS "-";
      Token.new_token Token.IDENT "b";
      Token.new_token Token.RBRACE "}";
      Token.new_token Token.EOF ""
    ]
    (To_test.lex {|
      let a = b
      fn (a, b) {
        a + b
      }
      1 < 2 == true
      1 > 2 == false
      macro (a, b) {
        a - b
      }
    |})

(* Run the tests *)
let () =
  Alcotest.run "Lexer" [
    "nextToken", [
      Alcotest.test_case "token list" `Slow test_same_tok;
      Alcotest.test_case "token list" `Slow test_same_tok2;
    ]
  ]
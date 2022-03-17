// sedlex lexer for simple function language

open Parser;
open Util;

let lex_error = (buf, msg) => {
    let loc = where(buf);
    Printf.eprintf("ERROR: %d:%d: Lexer: %s\n", loc.line, loc.col, msg);
    exit(1);
}

let rec token = (buf) => {
    switch%sedlex(buf) {
        | '+' => PLUS
        | '-' => MINUS
        | '(' => LPAREN
        | ')' => RPAREN
        | '{' => LBRACE
        | '}' => RBRACE
        | '=' => EQUALS
        | ';' => SEMI
        | "if" => IF
        | "else" => ELSE 
        | "def" => DEF
        | Plus('0'..'9') => {
            try (INT(int_of_string(Sedlexing.Utf8.lexeme(buf)))) {
                | Failure(_) => lex_error(buf, "int out of range") 
            }
        }
        | ((xid_start|'_'), Star(xid_continue)) => IDENT(Sedlexing.Utf8.lexeme(buf))
        | eof => EOF 
        | white_space => token(buf)
        | _ => lex_error(buf, "unknown token")
    }
};
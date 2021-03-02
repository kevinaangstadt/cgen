{
open Parser
exception Eof
}
rule token = parse
      [' ''\t']                     { token lexbuf }  (* skip blanks *)
    | ['\n']                        { Lexing.new_line lexbuf ; token lexbuf }
    | ['0'-'9']+ as lxm             { INT(int_of_string lxm) }
    | '+'                           { PLUS }
    | '-'                           { MINUS }
    | '('                           { LPAREN }
    | ')'                           { RPAREN }
    | '{'                           { LBRACE }
    | '}'                           { RBRACE }
    | '='                           { EQUALS }
    | ';'                           { SEMI }
    | ','                           { COMMA }
    | "if"                          { IF }
    | "else"                        { ELSE }
    | "def"                         { DEF }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as ident  { IDENT(ident) }
    | eof                           { EOF }

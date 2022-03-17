/* parser for simple language */
%{
open Ast
%}


%token <int> INT
%token <string> IDENT
%token IF ELSE DEF
%token LBRACE RBRACE LPAREN RPAREN EQUALS SEMI COMMA
%token EOF
%token PLUS MINUS
%left PLUS MINUS        /* lowest precedence */

%start program
%type <Ast.program> program

%%

program : def SEMI program EOF { $1 :: $3 }
        | def EOF { [$1] }
;

def : DEF IDENT LPAREN args RPAREN EQUALS exp { ($2, $4, $7) }
;

args : args2 { $1 }
     | { [] }
;

args2 : IDENT COMMA args2 { $1 :: $3 }
      | IDENT { [$1] }
;

exp : INT { Int($1) }
    | IDENT { Id($1) }
    | IF LPAREN exp EQUALS exp RPAREN LBRACE exp RBRACE ELSE LBRACE exp RBRACE { If($3, $5, $8, $12)}
    | exp PLUS exp { Add($1, $3) }
    | exp MINUS exp { Sub($1, $3) }
    | IDENT LPAREN exp_list RPAREN { Call($1, $3) }
;

exp_list : exp_list2 { $1 }
         | { [] }
;

exp_list2 : exp COMMA exp_list2 { $1 :: $3 }
          | exp { [$1] }
;
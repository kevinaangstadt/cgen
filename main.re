/*
 * Simple code generator for expression/function language
 * CS 364 Spring 2021
 * St. Lawrence University
 */


// check that there is one command line argument
if (Array.length(Sys.argv) != 2) {
    Printf.printf("Usage: %s source_file\n", Sys.argv[0]);
    exit(1);
}

// open the source file from the first command line argument
let fin = open_in(Sys.argv[1]);
let program = {
    // create lexer from the input file
    let buff = Lexing.from_channel(fin);

    // try to parse the program
    try(Parser.program(Lexer.token, buff)) {
        | _ => {
            // catch any parsing error and print out a helpful error message
            let curr = buff.Lexing.lex_curr_p;
            let line = curr.Lexing.pos_lnum;
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol;
            let tok = Lexing.lexeme(buff);
            Printf.printf("Parsing Error: \n");
            Printf.printf( "line: %d\n", line );
            Printf.printf( "column: %d\n", cnum );
            Printf.printf( "token: %s\n", tok);
            exit(1)
        }
    }
};


// open output file
let out_file = open_out("a.cl-asm");
Cgen.cgen_program(out_file, program);


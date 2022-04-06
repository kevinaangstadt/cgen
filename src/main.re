/*
 * Simple code generator for expression/function language
 * CS 364
 * St. Lawrence University
 */

open Util;


// check that there is one command line argument
if (Array.length(Sys.argv) != 2) {
    Printf.printf("Usage: %s source_file\n", Sys.argv[0]);
    exit(1);
}

// open the source file from the first command line argument
let fin = open_in(Sys.argv[1]);
let program = {
    // create lexer from the input file
    let buf = Sedlexing.Utf8.from_channel(fin);
    let lexer = Sedlexing.with_tokenizer(Lexer.token, buf);
    let parser = MenhirLib.Convert.Simplified.traditional2revised(Parser.program);

    // try to parse the program
    try(parser(lexer)) {
        | _ => {
            // catch any parsing error and print out a helpful error message
            let loc = where(buf);
            Printf.eprintf("Error: %d:%d: Parser: failed to parse\n", loc.line, loc.col);
            exit(1)
        }
    }
};

// print out the program
// TODO change this to generate code
print_endline(Ast.prog_to_string(program));


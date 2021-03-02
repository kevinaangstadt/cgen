/*
 * Simple code generator for expression/function language
 * CS 364 Spring 2021
 * St. Lawrence University
 */

open Printf

// These are our types for the AST
// note that expressions are a variant type...
// we can use a switch to determine which kind of expression we have
type program = list(definition)
and definition = (string, list(arg), exp)
and arg = string
and exp = 
    | Int(int)
    | Id(string)
    | If(exp, exp, exp, exp)
    | Add(exp, exp)
    | Sub(exp, exp)
    | Call(string, list(exp));


// Helper functions to print out our program
// Look! An example of using a switch to determin the expression type :-)

let rec exp_to_string = (e) => {
    switch(e) {
        | Int(i) => sprintf("%d", i)
        | Id(s) => s
        | If(e1, e2, e3, e4) => sprintf("if(%s = %s) { %s } else { %s }", 
            exp_to_string(e1),
            exp_to_string(e2),
            exp_to_string(e3),
            exp_to_string(e4)
        )
        | Add(e1, e2) => sprintf("%s + %s", exp_to_string(e1), exp_to_string(e2))
        | Sub(e1, e2) => sprintf("%s - %s", exp_to_string(e1), exp_to_string(e2))
        | Call(s, l) => switch(l) {
            | [hd, ...tl] => sprintf("%s(%s)", s, List.fold_left((acc, e) => sprintf("%s, %s", acc, exp_to_string(e)), exp_to_string(hd), tl))
            | [] => ""
        }
    }
};

let def_to_string = ( def : definition) => {
    let (name, args, e) = def;
    sprintf("def %s(%s) = %s", name, switch(args) {
        | [hd, ...tl] => List.fold_left((acc, e) => sprintf("%s, %s", acc, e), hd, tl)
        | [] => ""}, exp_to_string(e))
};

let prog_to_string = (prog : program) => {
    switch(prog) {
        | [hd, ...tl] => List.fold_left( (acc, e) => sprintf("%s;\n%s", acc, def_to_string(e)), def_to_string(hd), tl)
        | [] => ""
    }
    
};


// file containing code generation routines
open Ast;
open Printf;

// make a mutable variable to keep track of 
// the number of conditionals
let cond_count = ref(0);

// hash table for function names
let functions = Hashtbl.create(10);

// hash table for parameters
let params = Hashtbl.create(10);

// helper functions for stack
let push = (oc: out_channel, reg : string) => {
    fprintf(oc, "  push %s\n", reg)
};

let pop = (oc : out_channel, reg : string) => {
    fprintf(oc, "  pop %s\n", reg)
};

// code generation function for expressions
let rec cgen_exp = (oc : out_channel, e : exp) => {
    switch(e) {
        | Int(i) => {
            // integer constant
            // integer constant to end up in r0
            fprintf(oc, "  li r0 <- %d\n", i)
        }
        | Id(name) => {
            // access a argument by name
            // load this argument from memory
            let idx = Hashtbl.find(params, name);
            let loc = Hashtbl.length(params) - idx;
            fprintf(oc, "  ld r0 <- fp[%d]\n", loc);
        }
        | Add(e1, e2) => {
            // compute the value of e1
            cgen_exp(oc, e1);
            // push result onto the stack for now
            push(oc, "r0");

            // compute the value of e2
            cgen_exp(oc, e2);
            // e2 is now stored in r0
            // bring back the result of e1 into tmp reg
            // let's use r7
            pop(oc, "r7");

            // add r0 and r7 and store in r0
            fprintf(oc, "  add r0 <- r0 r7\n");
        }
        | Sub(e1, e2) => {
            // compute the value of e1
            cgen_exp(oc, e1);
            // push result onto the stack for now
            push(oc, "r0");

            // compute the value of e2
            cgen_exp(oc, e2);
            // e2 is now stored in r0
            // bring back the result of e1 into tmp reg
            // let's use r7
            pop(oc, "r7");

            // subtract r0 and r7 and store in r0
            fprintf(oc, "  sub r0 <- r7 r0\n");
        }
        | If(e1, e2, e3, e4) => {
            // if e1 = e2, then e3, else e4

            // get our condition id and increment the counter
            let cond = cond_count^;
            cond_count := cond_count^ + 1;


            // generate code for e1
            cgen_exp(oc, e1);
            push(oc, "r0");
            // generate code for e2
            cgen_exp(oc, e2);
            // pop value e1
            pop(oc, "r7");
            fprintf(oc, "  beq r0 r7 true_branch%d\n", cond);
            // fall through to the else next
            cgen_exp(oc, e4);
            fprintf(oc, "  jmp end_if%d\n", cond);

            // true branch
            fprintf(oc, "true_branch%d:\n", cond);
            cgen_exp(oc, e3);

            // end label
            fprintf(oc, "end_if%d:\n", cond);
        }
        | Call(name, exp_list) => {
            // name(exp_list)
            push(oc, "fp");
            List.iter( (e) => {
                cgen_exp(oc, e);
                push(oc, "r0");
            }, exp_list);

            // Call the function
            // label is stored in the functions hash table
            fprintf(oc, "  call %s\n", Hashtbl.find(functions, name));

            // returned from the function call
            fprintf(oc, "  li r7 <- %d\n", List.length(exp_list));
            fprintf(oc, "  add sp <- sp r7\n");

            // pop frame pointer
            pop(oc, "fp");
        }
    };
};

/*
 * Generate code for a function definition
 */
let cgen_def = (oc : out_channel, def : definition) => {
    // unpacked our definition tuple
    let (name, param_list, body) = def;

    // created a label for the function
    // add to our symbol table of function names
    let entry = sprintf("%s_entry", name);
    Hashtbl.add(functions, name, entry);

    // keep track of all in-scope parameters at this point
    List.iteri( (i, name) => {
        Hashtbl.add(params, name, i);
    }, param_list);

    // generate the label
    fprintf(oc, "%s:\n", entry);
    fprintf(oc, "  mov fp <- sp\n");

    // save our RA
    push(oc, "ra");

    // gnerate the code for the body of the function
    cgen_exp(oc, body); 

    pop(oc, "ra");

    // return from the function
    fprintf(oc, "  return\n");

    // clear out params
    Hashtbl.clear(params);
};

// generate code for built-in functions
let cgen_builtins = (oc : out_channel) => {
    // generate our print function
    Hashtbl.add(functions, "print", "print_entry");
    fprintf(oc, "print_entry:\n");
    fprintf(oc, "  mov fp <- sp\n");

    // body of "print"
    // load the argument to print
    fprintf(oc, "  ld r1 <- fp[1]\n");
    fprintf(oc, "  syscall IO.out_int\n");

    // return from the function
    fprintf(oc, "  return\n");
};

let cgen_program = (oc: out_channel, prog: program) => {
    
    // generates code for builtins
    cgen_builtins(oc);

    // iterate over program definitions
    // generate code for each of them
    List.iter( (def) => cgen_def(oc, def), prog);

    // start...calls main and exits
    fprintf(oc, "start:\n  call %s\n  syscall exit\n", 
                Hashtbl.find(functions, "main"));
};
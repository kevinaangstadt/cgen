open Ast;
open Printf;

let cond_cnt = ref(0);
let functions = Hashtbl.create(10);
let params = Hashtbl.create(10);
let push = (reg) => printf("  push { %s }\n", reg);
let pop = (reg) => printf("  pop { %s }\n", reg);

let acc = "r4";
let tmp = "r7";

let rec cgen_exp = (e : exp) => {
    // print a commment
    printf("  @ %s\n", exp_to_string(e));
    switch(e) {
        | Int(i) => printf("  mov %s, #%d\n", acc, i);
        | Id(s) => {
            let id = Hashtbl.find(params, s);
            let loc = Hashtbl.length(params) - id;
            // add to fp
            printf("  add %s, fp, #%d\n", acc, loc * 4);
            printf("  ldr %s, [%s]\n", acc, acc);
        }
        | Add(e1, e2) => {
            cgen_exp(e1);
            push(acc);
            cgen_exp(e2);
            pop(tmp);
            printf("  add %s, %s, %s\n", acc, tmp, acc);
        }
        | Sub(e1, e2) => {
            cgen_exp(e1);
            push(acc);
            cgen_exp(e2);
            pop(tmp);
            printf("  sub %s, %s, %s\n", acc, tmp, acc);
        }

        | If(e1, e2, e3, e4) => {
            let cond = cond_cnt^;
            incr(cond_cnt);

            cgen_exp(e1);
            push(acc);

            cgen_exp(e2);
            // e2 now in acc

            pop(tmp)
            // e1 now in tmp
            printf("  cmp %s, %s\n", acc, tmp);
            printf("  beq true_branch%d\n", cond);

            // fall through to else
            cgen_exp(e4);
            printf("  b end_if%d\n", cond);

            // make true branch
            printf("true_branch%d:\n", cond);
            cgen_exp(e3);

            printf("end_if%d:\n", cond);
        }
        | Call(name, exp_list) => {
            // push the current frame pointer
            push("fp");

            // iterate over the arguments
            List.iter( e => {
                cgen_exp(e);
                // push the answer to the new stack frame
                push(acc);
            }, exp_list);

            push("lr");
            // perform the call
            printf("  bl %s\n", Hashtbl.find(functions, name));
            pop("lr");
            // load size of the stack frame into tmp
            printf("  mov %s, #%d\n", tmp, List.length(exp_list)*4);
            // shift sp up by this
            printf("  add sp, sp, %s\n", tmp);

            // pop the old frame pointer
            pop("fp");
            
        }
    }
};

let cgen_def = (def : definition) => {
    let (name, param_list, body) = def;
    let entry = sprintf("%s_entry", name);
    Hashtbl.add(functions, name, entry);
    List.iteri( (i, e) => {
        Hashtbl.add(params, e, i);
    }, param_list);

    printf("%s:\n", entry);
    printf("  mov fp, sp\n");
    

    cgen_exp(body);

    
    printf("  bx lr\n");

    Hashtbl.clear(params);
};

let cgen_builtins = () => {
    // generate print
    Hashtbl.add(functions, "print", "print_entry");
    printf("print_entry:\n");
    printf("  mov fp, sp\n");
    printf("  add %s, fp, #4\n", acc);
    printf("  ldr r0, =print_fmt\n");
    printf("  ldr r1, [%s]\n", acc);
    push("lr");
    printf("  bl printf\n");
    pop("lr");
    printf("  bx lr\n")
}

let cgen_head = () => {
    printf(".cpu cortex-a53\n");
    printf(".global main\n");
    printf(".data \n");
    printf("print_fmt: .ascii \"%%d\\000\"\n");
    printf(".text\n");
    printf(".align 2\n");
}

let cgen_prog = (prog : program) => {
    cgen_head();
    cgen_builtins();
    List.iter(def => cgen_def(def), prog);
    printf("main:\n");
    push("lr");
    printf("  bl main_entry\n");
    pop("lr");
    printf("  bx lr\n");
};
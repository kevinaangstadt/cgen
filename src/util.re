type location = {
    line: int,
    col: int
};

let where = (buf) => {
    let (pos, _) = Sedlexing.lexing_positions(buf);
    let loc : location = {
        line: pos.pos_lnum, 
        col: pos.pos_cnum - pos.pos_bol + 1
    };
    loc
};
type instr =
    | ADD
    | MULT
    | PUSH of int
    | ABS

// Es S.1
type stack = ST of int list

let exec s i =
    match (i, s) with
    | (ADD, ST(op1 :: op2 :: tl)) -> ST(op1 + op2 :: tl)
    | (MULT, ST(op1 :: op2 :: tl)) -> ST(op1 * op2 :: tl)
    | (PUSH x, ST ls) -> ST(x :: ls)
    | (ABS, ST(op :: tl)) -> ST(abs op :: tl)
    | _ -> failwith "invalid stack"


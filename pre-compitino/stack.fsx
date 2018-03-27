#r "FsCheck"
open FsCheck

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

// Es S.2
type program = PR of instr list

let run prog =
    let rec auxRun (s, p) =
        match p with
        | PR([]) -> s
        | PR(i :: tl) -> auxRun (exec s i, PR(tl))
    match auxRun (ST([]), prog) with
    | ST([x]) -> x
    | _ -> failwith "invalid program"

// Es S.3
type exp =
    | C of int
    | Sum of exp * exp
    | Prod of exp * exp
    | Abs of exp

let rec eval t  =
    match t with
    | C n         -> n
    | Abs t      -> eval t |> abs
    | Sum(t1,t2)  -> eval t1  + eval t2
    | Prod(t1,t2) -> eval t1  * eval t2 ;;

let rec trans e =
    match e with
    | C x -> PR([PUSH x])
    | Sum(x, y) -> let (PR(r1), PR(r2)) = (trans x, trans y) in PR(r1 @ r2 @ [ADD])
    | Prod(x, y) -> let (PR(r1), PR(r2)) = (trans x, trans y) in PR(r1 @ r2 @ [MULT])
    | Abs x -> let (PR(r)) = trans x in PR(r @ [ABS])

let prop_equiv (e: exp) =
    trans e |> run = eval e

do Check.Quick prop_equiv


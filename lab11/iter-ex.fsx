#r "FsCheck"
open FsCheck

// Es 1
let itSum ls =
    let rec auxSum acc xs =
        match xs with
        | [] -> acc
        | hd :: tl -> auxSum (hd + acc) tl
    auxSum 0 ls

let itfSum ls = List.fold (fun x y -> x + y) 0 ls

let prop_sum ls =
    itSum ls = List.sum ls &&  List.sum ls = itfSum ls

do Check.Quick prop_sum

let itLength ls =
    let rec auxLength acc xs =
        match xs with
        | [] -> acc
        | hd :: tl -> auxLength (1 + acc) tl
    auxLength 0 ls

let itfLength ls = List.fold (fun x _ -> 1 + x) 0 ls

let prop_length ls =
    itLength ls = List.length ls && List.length ls = itfLength ls

do Check.Quick prop_length

let itMap f ls =
    let rec auxMap acc fx xs =
        match xs with
        | [] -> acc
        | hd :: tl -> auxMap (fx hd :: acc) fx tl
    auxMap [] f ls |> List.rev

let itfMap f ls = List.foldBack (fun x acc -> f x :: acc) ls []

let prop_map f ls =
    itMap f ls = List.map f ls && List.map f ls = itfMap f ls

do Check.Quick prop_map

// Es 2
let rec sumRange a b =
    match a <= b with
    | true -> a + sumRange (a + 1) b
    | _ -> 0

let itSumRange a b =
    let rec auxSumRange acc a b =
        match a <= b with
        | true -> auxSumRange (a + acc) (a + 1) b
        | _ -> acc
    auxSumRange 0 a b

let prop_sumRange a b =
    a <= b ==> lazy (
        let sum = [a .. b] |> List.sum
        sumRange a b = sum && sum = itSumRange a b
    )

do Check.Quick prop_sumRange

// Es 3
let rec fib n =
    match n with
    | x when x > 1 -> fib (x - 1) + fib (x - 2)
    | _ -> n

let itFib n =
    let rec auxFib acc1 acc2 m =
        match m with
        | 0 ->  acc1
        | _ -> auxFib acc2 (acc1 + acc2) (m - 1)
    auxFib 0 1 n

let prop_fib n =
    let smallPosIntGen n =
        Arb.filter (fun x -> 0 <= x && x <= n ) Arb.from<int>
    Prop.forAll (smallPosIntGen  n ) (fun k ->  fib k = itFib k )

do Check.Quick (prop_fib 30)

// Es 4
type 'a outcome =
    | Good of 'a list
    | Boom of 'a

let rec traverse pred xs =
    match xs with
    | [] -> Good([])
    | hd :: tl ->
        match (pred hd, traverse pred tl) with
        | (true, Good(ys)) -> Good(hd :: ys)
        | (true, x) -> x
        | _ -> Boom(hd)

let rec itTraverse pred xs =
    let rec auxTraverse acc pred xs =
        match xs with
        | [] -> Good(List.rev acc)
        | hd :: tl ->
            if pred hd then auxTraverse (hd :: acc) pred tl else Boom(hd)
    auxTraverse [] pred xs

let prop_traverse pred xs =
    traverse pred xs = itTraverse pred xs

do Check.Quick prop_traverse


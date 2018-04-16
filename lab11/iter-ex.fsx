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


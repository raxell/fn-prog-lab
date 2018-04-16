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


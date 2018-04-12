#r "FsCheck"
open FsCheck

// Es 1.1
let concat ls = List.foldBack (fun x acc -> x @ acc) ls []

let prop_concat ls =
    List.concat ls = concat ls

do Check.Quick prop_concat

// Es 1.2
let filter p ls = List.foldBack (fun x acc -> if p x then x :: acc else acc) ls []

let prop_filter p ls =
    List.filter p ls = filter p ls

do Check.Quick prop_filter

// Es 1.3
let unzip ls = List.foldBack (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) ls ([], [])

let prop_unzip ls =
    List.unzip ls = unzip ls

do Check.Quick prop_unzip

// Es 1.4
let sum_len ls = List.foldBack (fun x (sum, len) -> (x + sum, len + 1)) ls (0, 0)

let prop_sum_len ls =
    let (sum, len) = sum_len ls
    List.sum ls = sum && List.length ls = len

do Check.Quick prop_sum_len

// Es 2.1
let rec reduceBack f ls =
    match ls with
    | [hd] -> hd
    | hd :: tl -> f hd (reduceBack f tl)
    | _ -> failwith "The input list was empty"

let last ls = reduceBack (fun x y -> y) ls

// Es 2.2
let prop_reduceBack f ls =
    ls <> [] ==> lazy (List.reduceBack f ls = reduceBack f ls)

do Check.Quick prop_reduceBack


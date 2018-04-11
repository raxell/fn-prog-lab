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


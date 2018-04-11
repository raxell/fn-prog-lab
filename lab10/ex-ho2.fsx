#r "FsCheck"
open FsCheck

// Es 1.1
let concat ls = List.foldBack (fun x acc -> x @ acc) ls []

let prop_concat ls =
    List.concat ls = concat ls

do Check.Quick prop_concat


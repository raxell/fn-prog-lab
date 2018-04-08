#r "FsCheck"
open FsCheck// Es 9 2.1

let rec map f ls =
	match ls with
	| [] -> []
	| hd :: tl -> f hd :: map f tl

// Es 9 2.2
let l1 = [1..10]
let l2 = map (fun x -> x * x) l1
let l3 = map (fun x -> (x, if x % 2 = 0 then "pari" else "dispari")) l1

// Es 9 2.3
let names = [("Mario", "Rossi"); ("Anna Maria", "Verdi"); ("Giuseppe", "Di Gennaro")]
let names1 = map (fun (x, y) -> "Dott. " + x + " " + y) names

let prop_map f (ls: int list) =
    map f ls = List.map f ls

do Check.Quick prop_map

let prop_map_preserve_len f (ls: int list) =
     List.length (map f ls) =  List.length (List.map f ls)

do Check.Quick prop_map_preserve_len

// Es 3.1
let rec filter pred ls =
    match ls with
    | [] -> []
    | hd :: tl -> let x = filter pred tl in if pred hd then hd :: x else x

// Es 3.2
let mult3 n = filter (fun x -> x % 3 = 0) [1..n]

let prop_filter pred (ls: int list) =
    List.filter pred ls = filter pred ls

do Check.Quick prop_filter

let prop_filter_len pred (ls: int list) =
    List.length (filter pred ls) <= List.length ls

do Check.Quick prop_filter_len

// Es 4.1
let rec partition pred ls =
    match ls with
    | [] -> ([], [])
    | hd :: tl ->
        let (isTrue, isFalse) = partition pred tl
        if pred hd then (hd :: isTrue, isFalse) else (isTrue, hd :: isFalse)

// Es 4.2
let p1 = partition (fun x -> x % 3 = 0) [1..20]

// Es 4.3
let multNonmult3 n = partition (fun x -> x % 3 = 0) [1..n]

let prop_partition pred (ls: int list) =
    List.partition pred ls = partition pred ls

do Check.Quick prop_partition

let prop_partition_len pred (ls: int list) =
    let (xs, ys) = partition pred ls
    xs @ ys |> List.length = List.length ls

do Check.Quick prop_partition_len

let prop_partition_app pred (ls: int list) =
    let (xs, ys) = partition pred ls
    xs @ ys |> List.sort = List.sort ls

do Check.Quick prop_partition_app

// Es 5.1
let divisori n = filter (fun x -> n % x = 0) [1..n]


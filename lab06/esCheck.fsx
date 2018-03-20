#r "FsCheck";;
open FsCheck;;

// 1.1
let rec rmEven ls =
    match ls with
    | [] -> []
    | hd :: tl ->
        match hd % 2 with
        | 0 -> rmEven tl
        | _ -> hd :: rmEven tl

let rec containsOddsOnly ls =
    match ls with
    | [] -> true
    | hd :: tl ->
        match hd % 2 with
        | 0 -> false
        | _ -> containsOddsOnly tl

let prop_oddsOnly xs =
    rmEven xs |> containsOddsOnly

do Check.Quick prop_oddsOnly

// 1.2
let rec rmOddPos ls =
    match ls with
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: hd2 :: tl -> hd1 :: rmOddPos tl

let prop_halved xs =
    rmOddPos xs |> List.length = (List.length xs + 1) / 2

do Check.Quick prop_halved

// Subset check
let prop_subset f xs =
    Set.isSubset (f xs |> Set.ofList) (Set.ofList xs)

do Check.Quick (prop_subset rmEven)
// do Check.Quick (prop_subset rmOddPos)

// 1.3
let rec split ls =
    match ls with
    | [] -> ([], [])
    | [hd] -> ([hd], [])
    | hd1 :: hd2 :: tl ->
        let (even, odd) = split tl
        (hd1 :: even, hd2 :: odd)

let rec merge (xs, ys) =
    match (xs, ys) with
    | ([], []) -> []
    | (hd :: tl, []) -> [hd]
    | ([], hd :: tl) -> [hd]
    | (hd0 :: tl0, hd1 :: tl1) -> hd0 :: hd1 :: merge (tl0, tl1)

let prop_inverse xs =
    split xs |> merge = xs

do Check.Quick prop_inverse

// downto0 check
let rec downto0 n =
    match n with
    | 0 -> [0]
    | x -> x :: downto0 (n - 1)

let prop_eqRange n =
    n >= 0 ==> lazy (downto0 n = [n .. -1 .. 0])

do Check.Quick prop_eqRange
#r "FsCheck";;
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

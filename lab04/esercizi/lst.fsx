// Es 2.1
let rec rmEven ls =
    match ls with
    | [] -> []
    | hd :: tl ->
        match hd % 2 with
        | 0 -> rmEven tl
        | _ -> hd :: rmEven tl

// Es 2.2
let rec rmOddPos ls =
    match ls with
    | [] -> []
    | [hd] -> [hd]
    | hd1 :: hd2 :: tl -> hd1 :: rmOddPos tl

// Es 2.3
let rec split ls =
    match ls with
    | [] -> ([], [])
    | [hd] -> ([hd], [])
    | hd1 :: hd2 :: tl ->
        let (even, odd) = split tl
        (hd1 :: even, hd2 :: odd)

// Es 2.4
let rec cmpLength x =
    match x with
    | ([], []) -> 0
    | (hd :: tl, []) -> 1
    | ([], hd :: tl) -> -1
    | (hd0 :: tl0, hd1 :: tl1) -> cmpLength (tl0, tl1)

// Es 2.5
let rec remove (x, ls) =
    match ls with
    | [] -> []
    | hd :: tl ->
        match hd = x with
        | true -> remove (x, tl)
        | _ -> hd :: remove (x, tl)

let rec removeDup ls =
    match ls with
    | [] -> []
    | hd :: tl -> hd :: removeDup (remove (hd, tl))

// Es 2.6
let rec downto0 n =
    match n with
    | 0 -> [0]
    | x -> x :: downto0 (n - 1)

let rec upto n =
    match n with
    | 0 -> [0]
    | x -> upto (n - 1) @ [x]

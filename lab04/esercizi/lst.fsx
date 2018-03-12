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

let rec cmpLength (ls0,ls1) = 0

let rec remove (x, ls) = []

let rec removeDup ls =  []

let rec downto0 n = []

let rec upto n = []

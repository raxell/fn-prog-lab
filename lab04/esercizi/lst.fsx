let rec rmEven ls =
    match ls with
    | [] -> []
    | hd :: tl ->
        match hd % 2 with
        | 0 -> rmEven tl
        | _ -> hd :: rmEven tl

let rec rmOddPos ls = []

let rec split ls = [],[]

let rec cmpLength (ls0,ls1) = 0

let rec remove (x, ls) = []

let rec removeDup ls =  []

let rec downto0 n = []

let rec upto n = []

#r "FsCheck"
open FsCheck

// Es 0
type valV = { studente: string; voto: int }
type valG = { studente: string; giudizio: string }

// Es 1
let valuta { studente = s; voto = v } =
    let r = { studente = s; giudizio = "" }
    match v with
    | x when x > 27 -> { r with giudizio = "ottimo" }
    | x when x > 22 -> { r with giudizio = "buono" }
    | x when x > 17 -> { r with giudizio = "sufficiente" }
    | _ -> { r with giudizio = "insufficiente" }

// Es 2
let rec valutaList ls =
    match ls with
    | [] -> []
    | hd :: tl -> valuta hd :: valutaList tl

// Es 3
let rec creaValList (s, v) =
    match (s, v) with
    | (hd0 :: tl0, hd1 :: tl1) -> { studente = hd0; voto = hd1 } :: creaValList (tl0, tl1)
    | _ -> []

// Es 4
let media ls =
    let f (sum, count) { voto = v } = (sum + v, count + 1)
    match List.fold f (0, 0) ls with
    | (sum, count) when count > 0 -> float sum / float count
    | _ -> 0.

// Es 5
let rec separa ls =
    match ls with
    | [] -> ([], [])
    | hd :: tl  ->
        let (b, p) = separa tl
        if hd.voto < 18 then (hd :: b, p) else (b, hd :: p)

let ``due liste risultato hanno stessi elementi di vs`` vs =
    let (x, y) = separa vs
    Set.ofList vs = Set.ofList (x @ y)

do Check.Quick ``due liste risultato hanno stessi elementi di vs``


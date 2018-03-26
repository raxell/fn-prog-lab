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


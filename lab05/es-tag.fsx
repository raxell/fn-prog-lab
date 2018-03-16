type figura =
    | Rettangolo of float * float
    | Quadrato of float
    | Triangolo of float * float

let area fig =
    match fig with
    | Rettangolo(b,h) -> b * h
    | Quadrato l -> l * l
    | Triangolo(b,h) -> (b * h) / 2.

// Es. 5.1
let areaOpt fig =
    match fig with
    | Rettangolo(b, h) when b >= 0. && h >= 0. -> Some (area fig)
    | Quadrato l when l >= 0. -> Some (area fig)
    | Triangolo(b, h) when b >= 0. && h >= 0. -> Some (area fig)
    | _ -> None


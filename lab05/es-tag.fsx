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

// Es. 5.2
let printArea fig =
    let areaFig = areaOpt fig
    match areaFig with
    | Some x -> "area: " + string x
    | None -> "la figura non e' ben definita"

// Es. 5.3
let sommaArea (fig1, fig2) =
    match (areaOpt fig1, areaOpt fig2) with
    | (Some x, Some y) -> Some (x + y)
    | _ -> None

// Es 5.4
let rec sommaAreaList figs =
    match figs with
    | [] -> 0.
    | hd :: tl ->
        match areaOpt hd with
        | Some x -> x + (sommaAreaList tl)
        | _ -> 0. + (sommaAreaList tl)


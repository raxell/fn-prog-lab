// Es 9 2.1
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


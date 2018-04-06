// Es 9 2.1
let rec map f ls =
	match ls with
	| [] -> []
	| hd :: tl -> f hd :: map f tl


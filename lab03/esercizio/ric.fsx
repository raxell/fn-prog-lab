// Esercizi su FUNZIONI RICORSIVE
// mock file:


//  gcd : int * int -> int
let rec gcd (m, n) =
    match m with
    | 0 -> n
    | _ -> gcd(n % m, m)

// simplify : int * int -> int * int
(*  Dati due interi a >= 0 e  b>0, simplify semplifica la frazione a/b
    Piu' precisamente, simplify (a,b) = (c,d) se e solo se
    c/d e' la frazione ottenuta semplificando  a/b

*)

let simplify (a , b) =
    let d = gcd (a, b) in
        (a / d, b / d)


// sum1 : int -> int
// dato n >= 0,  calcola la somma  dei numeri compresi  fra 0 e n

let rec sum1 n =
    match n with
    | 0 -> 0
    | m -> m + sum1 (m - 1)


// sum2 : int * int -> int
(*
   Dati m e n tali che n > m,
  calcola la somma dei numeri compresi fra m e n, altrimenti 0       
*)

let rec sum2 (m,n) =
    match n >= m with
    | false -> 0
    | _ -> m + sum2 (m + 1, n)


let rec fexp ( bse : float, k ) =
    match k with
    | 0 -> float 1
    | n ->
        match n % 2 with
        | 0 -> fexp (bse * bse, n / 2)
        | _ -> bse * fexp (bse * bse, n / 2)



//  fib : int -> int
// dato n >= 0, fib n e' l' n-esimo  numero di Fibonacci      

let rec fib m =
    match m with
    | 0 -> 1
    | 1 -> 1
    | n -> fib (n - 1) + fib (n - 2)





#r "FsCheck"
open FsCheck


(*
   FUNZIONI HIGHER-ORDER (LIST LIBRARY)
   ====================================


*   List.map :  ('a -> 'b) -> 'a list -> 'b list

    Dati f : 'a->'b  e   [x0; x1 ; ... ; xn] : a' list

      List.map f  [x0; x1 ; ... ; xn] = [ f x0 ; f x1 ; ... ; f xn]

Esempio:
*)
let m = List.map (fun n -> n*n) [1 .. 10] ;;
//  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]

let rec PosList ls =
  match ls with
    | [] -> []
    | x :: xs -> (x > 0) :: PosList xs

let alist = PosList [4; -5; 6];;


let isPosListMap xs = List.map (fun x -> x > 0) xs

// più brevemente (point-wise)
let isPosListMap2 = List.map (fun x -> x > 0) 

let prop_ispos xs =
  isPosListMap xs = PosList xs
do Check.Quick prop_ispos

let rec addElems ls =
  match ls with
    | [] -> []
    | (x,y)::zs -> (x+y)::addElems zs;;

let a = addElems [(1,2) ;(3,4)];;

let addElemsMap  = List.map (fun (x, y) -> x + y) 

let prop_addel xs =
  addElemsMap xs = addElems xs
do Check.Quick prop_addel


(*
 List.filter : ('a -> bool) -> 'a list -> 'a list

  Dati un predicato pred : 'a-> bool e una lista ls  : 'a list

  List.filter pred ls =  lista degli x in ls per cui 'pred x' vale
*)

let chs= List.filter System.Char.IsLower  ['A'..'z']

// altri esempi:

let f2 = List.filter (fun x -> x % 2 = 0) [1..10] ;; // [2; 4; 6; 8; 10]  
let f3 = List.filter (fun x -> x > 10 ) [1..10] ;;   // []  


// Cominciare qui!!


// definzione esplicita di intersezione

let rec interR ys ls  =
  match ls with
    | [] -> []
    | x :: xs when List.contains x ys -> x :: interR ys xs
    | _ :: xs -> interR ys xs 

let iie = interR [1..10] [5..15]

// higher-order?






let inter xs ys = List.filter (fun x -> List.contains x ys) xs

let ii = inter [1..10] [5..15]

// opzionale
// e se facessimo loop su xs?
let inter2 xs ys = List.filter (fun x -> List.contains x xs) ys
let ii2 = inter2 [1..10] [5..15]

// sicuri? In fondo intersezione è commutativa ...

let prop_inter (xs : int list) ys =
 inter xs ys = inter2 xs ys  
do Check.Quick prop_inter


// Ah! Bisogna non avere ripetizioni, altrimenti operazione non è commutativa
// sistemo convertendo i risultati in insiemi (che implicitamente ordina anche)
let prop_inter_comm2 (xs : int list) ys =
  (inter xs ys |> Set.ofList) = (inter2 ys xs |> Set.ofList)
do Check.Quick prop_inter_comm2



// Come definire List.contains con un combinatore?

(*   List.exists : ('a -> bool) -> 'a list -> bool

    Dati pred : 'a-> bool  e  ls : 'a list

      List.exists pred ls = true      se esiste x in ls tale che 'pred x' e' true     
                            false     altrimenti (per ogni x in ls, 'pred x' e' false)
*)

let rec exists p ys =
  match ys with
    | [] -> false
    | x::xs -> p x || exists p xs;;



let Lcontains x xs = List.exists (fun y -> y=x) xs;;

(* List.exists è astrazione di questo pattern:
let rec  Lcontains x = function
  | [] -> false
  | y :: ys -> (x = y) || Lcontains x ys

   *)    

// altri esempi:

let e1 = List.exists (fun n -> n % 2 = 0 ) [1 .. 10] ;;  // true  
let e2 = List.exists (fun n -> n > 100 ) [1 .. 10] ;;    // false 


(*   List.forall : ('a -> bool) -> 'a list -> bool

    Dati pred : 'a-> bool e  ls  : 'a list

      List.forall pred ls = true      se, per ogni x in ls, 'pred x' e' true     
                            false     altrimenti (esiste  x in ls tale che 'pred x' e' false)
*)
// Esempi:

let a1 = List.forall (fun n -> n  < 11 ) [1 .. 10];;     // true
let a2 = List.forall (fun n -> n % 2 = 0 ) [1 .. 10];;   // false  



(*

ESERCIZIO

 Definire la funzione exists analoga a List.exists usando
 List.forall;Usare le equivalenze di De Morgan:

  exists x P(x)  <==>  ~( forall x ~P(x) )
*)

let existsdm p =  List.forall  p >> not  >> not

// notare 1. definizione point-wise 2. uso di composizione funzionale
// si legge per associazione a sinistra
// (List.forall  (p >> not))  >> not


// esercizio da libro: due liste sono disgiunte (opzionale)

// esplicita: servono due loop
let rec disjR xs ys =
  let rec loop x = function
    | [] -> true
    | y :: ys when y <> x -> loop x ys
    | _ -> false

  match xs with
    | [] -> true
    | x :: xs' -> loop x ys && disjR xs' ys

let d3 = disjR [1..10] [20..24]
let d4 = disjR [1..10] [10..24]

// intermedia

let rec disjR1 xs ys =
  match xs with
    | [] -> true
    | x :: xs' -> not (List.contains x ys) && disjR xs' ys


// completamente ho 
let disj xs ys =
      List.forall (fun x -> List.forall (fun y -> x <> y) ys) xs

let d1 = disj [1..10] [20..24]
let d2 = disj [1..10] [10..24]


let prop_disj (xs : int list) ys =
 disj xs ys = disjR xs ys  
do Check.Quick prop_disj



// %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
// FOLD


//  sumlist : int list -> int
let rec sumlist = function
  [] -> 0
  | x :: xs -> x + sumlist xs
  
// prodlist : int list -> int
let rec prodlist = function
  [] -> 1
  | x :: xs -> x *  prodlist xs

// en passant, un altro modo di definire il fattoriale
let fact n =
  [1..n] |> prodlist

// val lenlist : 'a list -> int
let rec lenlist = function
  [] -> 0
  | _ :: xs -> 1 + lenlist xs


(*
//  Generalizziamo:

L'idea è rimpiazzare il cons delle lista con una funzione (binaria)
 arbitraria e il nil con un elemento (il neutro della funzione stessa).
 
Si consideri la lista

 1 ::( 2 :: (3 :: [])).

Per fare la somma di una lista usiamo + per :: e 0 per [] 

 1 + (2 + (3 + 0)).

Per fare il prodotto di una lista usiamo * per :: e 1 per [] 

 1 * (2 * (3 * 1)).

 Per la lunghezza, dove (+1) è la funzione binaria che ignora primo
 argomento e incrementa il secondo. Il neutro è 0.

 _ (+1) (_ (+1) ( _ (+1) 0)) 


 List.foldBack  : (('a -> 'b -> 'b) -> 'a list -> 'b -> 'b)
-  una funzione f a due argomenti 
-  una lista ls = [x0; ... ; x(n-1)] 
-  un valore inizale e : 'b

foldBack f e [ x0 ; x1 ; ... ; x_n-1) ] =
     f x0 ( f x1 ...   (f x_n-2 (f x_n-1 e))  ... )
*)

// (+) : int -> int -> int, xs : int list, 0 : int
let sumlistF xs  = List.foldBack (+) xs 0

// più esplicitamente
let sumlistF1 xs  = List.foldBack (fun x y -> x + y) xs 0

let prodlistF xs = List.foldBack ( * ) xs 1

let lenlistF xs = List.foldBack (fun  _ n -> 1 + n) xs 0   ;;

// la definizione esplicite di foldBack
let rec foldBack f xs e =
  match xs with
    | [] -> e
    | y :: ys -> f y (foldBack f ys e)

// ----------------------------------------------------------------------------
// Fold è "universale" nel senso che può definire qualunque funzione
    // definita per ricorsione su liste. Qualcuna è immediata, altre richiede qualche manipolazione

    
    // Consiglio: "A tutorial on the universality and expressiveness of
    // fold." GRAHAM HUTTON University of Nottingham, Nottingham, U
    // http://www.cs.nott.ac.uk/~pszgmh/fold.pdf

    
// Map definita via fold

let map f ls = List.foldBack (fun x xs -> f x :: xs) ls [] ;;

(*
come funziona astrazione: si prenda la definizione di map

let rec map f xs =
  match xs with
    | [] -> []
    | y :: ys -> (f y) :: (map f ys)


- nel caso base ritorna [], quindi "e" sarà []

- nel passo sto iterando una funzione che applica la f alla testa e ricorre
   fun h tl -> (f h) :: tl , quindi rimpiazzo la chiamata rcorsiva con una nuova variabile tl
   *)


// append
let app xs ys = List.foldBack (fun h tl ->  h :: tl) xs ys ;;   

// simile: ricorsione su xs, la funzione iterata la cons, ma il neutro è la lista di ritorno ys!

// ------------------------------------

   (* Esiste anche una funzione duale

   List.fold ; : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a

   che invece associa a sinistra

    fold f e [ x0 ; x1 ; ... ; x_n-1 ] =
     f ( ...  f(f (f e x0) x1) x2  ...  ) x_n-1
     *)

let rec fold f e xs  =
  match xs with
    | [] -> e
    | y :: ys -> fold f (f e y) ys 


//  f: ('a -> 'b -> 'a) -> e: 'a -> xs : 'b list -> 'a

let sumlist1 xs  = List.fold (+) 0 xs 

// Notate che prende prima il neutro e poi la lista e questo ci permette di riformulare point-wise

let sumlist2  = List.fold (+) 0     

(*
 Se l'operazione e il suo neutro sono un **monoide**, la fold la
 foldBack sono equivalenti,


Monoide: insieme M con operazione @ ed elemento  "e", tale che

- @ è associativa
- "e" suo neutro: e @ m = m @ e = m

In questo caso x1 @ (x2 @ (x3 @ e)) = (((e @ x1) @ x2 ) @ x3)


La fold è preferibile in quanto tail-recursive
 e quindi occupa spazio constante in memoria. In sostanza, la fold è da usare
 quando non vi interessa che gli elementi siano rigirati


  *)


// se non associativa, danno risultati diversi
let r1 = List.fold (-) 0 [1..3]
//  (( 0 - 1) - 2) - 3 = -6
let r2 = List.foldBack (-)  [1..3] 0
//  1 - ( 2 - 3 - 0) = 2

// ecco la reverse    
let rev ls = List.fold  ( fun  tl  h  -> h :: tl ) [] ls ;;

// se la scrivessi come una foldBack cosa ottengo?
let f ls = List.foldBack  ( fun  h tl  -> h :: tl ) [] ls;;

(* MORALE

Nei casi  monidali (es., le funzioni sum, prod, length etc), si puo'
usare indifferentemente fold o foldBack.  
In tali casi l'uso di fold e' preferibile, in quanto fold, a differenza di foldBack,
e' definita tramite tail recursion.

Ci sono invece casi in cui occorre necessariamente usare fold
(es. rev) oppure foldBack (es. map).

*)


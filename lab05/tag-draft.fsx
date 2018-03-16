//  ******  TAGGED VALUES  ******

(*

 Libro di testo HR: paragrafi  3.8, 3.9, 3.11 

*)   

(*

I *tagged value*  sono usati per raggruppare valori di forma diversa in un unico tipo.


In F#  una collezione di tagged value  e' definita mediante una dichiarazione di tipo 

   type TV  = .....

Maggiori dettagli negli esempi presentati sotto.


Noti in ADA, Pascal, VBasic come "variant records", sono chiamati  "(algebraic) datatypes" in FP

*)


(******  TIPI ENUMERATIVI  ******)

(*

I tipi enumerativi sono il caso piu' semplice di tagged value (caso degenere).


*)

type colore = Rosso | Blu | Verde 

(*

Viene definito il tipo colore i cui elementi sono  Rosso, Blu e Verde.

 Rosso, Blu e Verde sono detti *costruttori (constructor)*.

In questa caso  abbiamo costruttori senza argomenti, che possono
essere visti come delle costanti di tipo colore.

NOTE
===

1)  I costruttori *devono* essere in maiuscolo

2)  I tipi enumerativi qui introdotti non vanno confusi con gli enum type di F# e di C#
    (che non vedremo nel corso):

    https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/enumerations

*)   

(**  PATTERN MATCHING  **)

(* Definiamo la  funzione

     valore : colore -> int

che  associa a ogni colore un intero nel modo seguente:

    Rosso  ---> 1
    Blu    ---> 2
    Verde  ---> 3 

*)

let valore col =
    match col with  // pattern matching su un valore di tipo colore
    | Rosso -> 1
    | Blu   -> 2
    | Verde -> 3 
// val  valore : colore -> int 

// Esempio di applicazione della funzione valore

valore Rosso 
// val it : int = 1

(*

Esercizi
^^^^^^^^

1) Dato il tipo

type month = January | February | March | April | May | June | July
             | August | September| October | November | December 


che rappresenta i mesi in un anno, definire la funzione

     daysOfMonth : month -> int

che calcola il numero di giorni in un mese (anno non bisestile).

Ad esempio:

daysOfMonth March 
// val it : int = 31

2) Consideriamo  il tipo 

 type myBool = True | False 

Definire le funzioni:

*  myNot : myBool -> myBool  // operatore Not su myBool
  
*  myBool2bool : myBool -> bool   // trasforma un bool nel corrispondente valore myBool

Esempi:

myNot False 
// val it : myBool = True

myBool2bool False 
// val it : bool = false

*)


(****  TAGGED VALUES  ****)

(**

Vediamo ora esempi di tagged value non  non degeneri.

Esempio
^^^^^^^
Definiamo il tipo figura in cui ogni elemento puo' essere:

- un Rettangolo di cui si specifica la misura della base e dell'altezza (coppia di float)

   OPPURE
 
- un Quadrato di cui si specifica la misura del lato (un float)

   OPPURE

- un Triangolo di cui si specifica la misura della base e dell'altezza (coppia di float)

*)

// Definizione del datatype figura

type figura = 
   | Rettangolo of  float * float      // (base, altezza)
   | Quadrato   of  float              // lato
   | Triangolo  of  float * float      // (base, altezza)

(*

Il tipo figura e' definito dai seguenti tre costruttori:

 Rettangolo :  float * float -> figura 
 Quadrato   :  float -> figura 
 Triangolo  :  float * float -> figura 

Ciascun  costruttore e' una funzione che costruisce un valore (tagged value) di tipo figura.

NOTA
====

In Java l'esempio si implementa definendo una classe astratta Figura
che ha come sottoclassi concrete le classi Rettangolo, Quadrato, Triangolo.

Essendo la classe Figura astratta, non e' possibile istanziarla.
Gli unici oggetti di tipo Figura devono essere istanze delle classi  Rettangolo, Quadrato.  Triangolo
(o di eventuali altre sottoclassi concrete della classe Figura).


*)


(*
Gli argomenti dei costruttori devono avere il tipo richiesto.
Ad esempio,  il termine

Quadrato 5

non e' corretto, in quanto l'argomento di Quadrato deve avere tipo float.
La valutazione produce l'errore:

This expression was expected to have type
    float    
but here has type
    int    
*)   

// Definizione di figure che useremo piu' avanti

let rett = Rettangolo (4. , 5.)   // 4. e' la costante 4.0
//  rett : figura = Rettangolo (4.0,5.0) 

let quad1 = Quadrato 10.  
// val quad1 : figura = Quadrato 10.0

let quad2 = Quadrato 5.  ;;
// quad2 : figura = Quadrato 5.0

let tr = Triangolo (5.,3.) ;;
// val tr : figura = Triangolo (5.0,3.0)

(**  PATTERN MATCHING  con tagged value  **)


(*

Definiamo la funzione area che calcola l'area di una figura fig.
Si *assume*  che fig sia una figura ben definita (dimensioni non  negative).
Questo significa che non e' necessario fare controlli sull'argomento
cui la funzione e' applicata.

La funzione area ha tipo

  area : figura -> float 

Occorre fare pattern matching sull'argomento fig  della funzione.
Per definizione, fig puo' avere una delle seguenti tre forme:

  Rettangolo(b,h)     // rettangolo con base b e altezza h
  Quadrato l          // quadrato di lato l
  Triangolo(b,h)      // triangolo con base b e altezza h

Per ciascuno dei tre casi, va calcolato il valore dell'area.

*)

let area fig =
    match fig with
    | Rettangolo(b, h) -> b * h
    | Quadrato l -> l * l
    | Triangolo (b, h) -> b * h / 2.
 // val area : figura -> float 


// Esempi

let aRett = area rett   // rett = Rettangolo  Rettangolo (4. , 5.)
// val aRett : float = 20.0

let aQ1 = area quad1  //  quad1 = Quadrato 10. 
// val aQ1 : float = 100.0

let aQ2 = area quad2  //  quad2 = Quadrato 5.  
// val aQ2 : float = 25.0

let aTr = area tr // tr = Triangolo (5.,3.)
// val aTr : float = 7.5

// *** Esempio di tagged value con un solo costruttore 

// definiamo un tipo che rappresenta una figura colorata

type figuraColorata =  Col of figura * colore 

(* Il tipo  figuraColorata  ha un solo costruttore:

   Col : figura * colore  ->  figuraColorata 
 
*)   


let fc1 = Col (quad1, Rosso) 
// val fc1 : figuraColorata = Col (Quadrato 10.0,Rosso)
// notare che le definizioni sono espanse


(*

Esercizio
^^^^^^^^^

Definire la funzione

   valFigura : figuraColorata -> float

che calcola il valore di una figura colorata figCol definito come segue:  

   valFigura (figCol)  =  area(figCol) *  valore( colore(figCol) )  

dove il valore di un colore e' determinati dalla funzione valore : colore -> int
definita sopra.

Si *assume*  che figCol sia ben definita (dimensioni non negative).

Mediante opportune chiamate alla funzione valFigura, determinare il valore
di un quadrato di lato 3 per ciascuno dei tre possibili colori (Rosso, Blu, Verde).

*)


let valFigura figCol =
    match figCol with
    | Col (fig, col) -> area fig * float (valore col)
 // valFigura : figuraColorata -> float


let v1 = valFigura ( Col (Quadrato 3.0 , Rosso) )  // Rosso ---> 1
// val v1 : float = 9.0
// NOTA: le parentesi sono obbligatorie, altrimenti l'espressione non e' interpretata nel modo desiderato


let v2 = valFigura ( Col (Quadrato 3.0 , Blu) )    // Blu ---> 2
// val v2 : float = 18.0

let v3 = valFigura ( Col (Quadrato 3.0 , Verde ) ) // Verde ---> 3
// val v3 : float = 27.0

(*
I tag value permettoni di introdurre meccanismi di astrazione che rendano il codice piu' sicuro e robusto.

Esempio
^^^^^^^

Definiamo i tipi voto e matricola per rappresentare i voti e le matricole
di studenti (numei interi).
*)

// ** Soluzione 1 (poco astratta)  **

type  voto1 = int   // voti
type  matricola1 = int  // matricole

(*
Tali definizioni sono trattate come  *alias* e sono  rimosse dal compilatore.
Questo significa che voti e matricole sono visti semplicemente come interi
(in altri termini, non vi e' data encapsulation).
*)

// esempio di funzione che opera su voti e matricole
let print1 (v : voto1, m : matricola1) =
  "Voto: " + ( string v ) + " -- matricola: " + (string m )
// val print1 : v:voto1 * m:matricola1 -> string

// definizione di valori di tipo voto1 e matricola1
let v1 = 24 : voto1     // cast per forzare l'intero 24 al tipo voto1 
// val v1 : voto1 = 24
let m1 = 123456 : matricola1
// val m1 : matricola1 = 123456

print1 (v1, m1 )    
// val it : string = "Voto: 24 -- matricola: 123456"

print1  (m1, v1)  
//  val it : string = "Voto: 123456 -- matricola: 24"

(*
Notare che la seconda chiamata e' stata accettata,
nonostante i tipi non siano usati in modo coerente.
*)   


// ** Soluzione 2:  definizione safe dei tipi voto e matricola usando tagged value **

type  voto2 =  V of int        // costruttore V  definisce un voto
type  matricola2 = M of int    // costruttore  M definisce una matricola

// ora non e' piu' possibile confondere voti e matricole (costruttori distinti)!

let print2 (v, m) =
    match (v, m) with
    | (V voto, M mat) ->
        "Voto: " + (string voto) + " -- matricola: " + (string mat)
 //val print2 : v:voto2 * m:matricola2 -> string


let v2 =  V 24    // voto 24
// val v2 : voto2 = 24
let m2 =  M 123456   // matricola 123456
// val m1 : matricola2 = 123456

print2 (v2, m2) 
// val it : string = "Voto: 24 -- matricola: 123456"

// Rispetto alla soluzione precedente, se gli argomenti sono invertiti si ha errore di tipo:

print2 (m2, v2)  // ERRATA !!!
// This expression was expected to have type voto2   but here has type matricola2    

// Altro esempio significativo con unita' di misura:

type miles = float
type kilometres = float

(* E' praticamente garantito che essendo tutti float, a un certo punto
 li confonderò, cosa che successe nel disastro del NASA’s Mars Climate
 Orbiter, andato perduto nel 1999 per una confusione tra miglia e
 kilometri http://mars.jpl.nasa.gov/msp98/news/mco991110.html

Non vi gantisco che questo vi manderà su Marte, ma è codice meglio scritto ...
*)

type miles2 = M of float
type kilometres2 = K of float

(*

 In F# esistono anche le unità di misura

 https://blogs.msdn.microsoft.com/andrewkennedy/2008/08/29/units-of-measure-in-f-part-one-introducing-units/

*)
   
(*

OPTION TYPE
^^^^^^^^^^^
In F#  e' definito la tagged union *polimorfa*

type 'a option =   
    | None
    | Some of 'a

(Maybe in Haskell)

Molto utile per esprimere funzioni *PARZIALI *, ossia le funzioni che non sono definite
su tutti gli elementi del dominio.

Sintassi alternativa generale per tipi polimorfi (angle brackets)

 type option<'a> =       
   | Some of 'a        
   | None   

   *)

(* Esempio 1
   ^^^^^^^^^^ 

Consideriamo la funzione

let rec fact n =
    match n with
        | 0 -> 1                     // fact 0 =  1 
        | _ -> n * fact ( n-1 );;    // fact n =  m  * fact ( n-1 )

che calcola il fattoriale di m.
La funzione fact ha tipo

  fact :  int -> int

ma  il fattoriale di n e' definito solo per n >=0.
Quindi, fact e' una funzione *parziale*.

Come gestire il caso in cui la funzione e' applicata a un intero n<0 ?

Sono possibili almeno due soluzioni:

(i) Viene sollevata una eccezione (vedremo i dettagli e  come gestirle in una prossima lezione)

*)

// fattoriale che solleva eccezione se argomento n e' negativo 
let rec gfact n =
    match n with
    | 0 -> 1
    | _ ->
        if  n > 0 then n * gfact (n-1)  else failwithf "Negative argument %i to fact" n

gfact ( -10 );;

(*
(ii) Si definisce la funzione in modo che restituisca un valore di tipo 'int option'.

Piu' precisamente, definiamo la funzione

  factOpt : int -> int option

Il risultato R dell'applicazione

   factOpt n

e' un tagged value di tipo 'int option' della forma 'Some k' oppure 'None'.

- Se R = Some k, allora  k e' il fattoriale di n
  (in questo caso si ha n >= 0). 
  
- Se R = None, il fattoriale non e' definito
  (si e' applicata la funzione a  n < 0).

In altri termini:

- se la chiamata 'factOpt n' restituisce None, significa che  fattoriale di n non e' definito,

- se la chiamata 'factOpt n' restituisce 'Some k', significa che il fattoriale di n e' definito
  e il risultato e' k.
  Per 'estrarre' il valore di k, e' fondamentale l'uso del pattern matching.

*)


// (ii): fattoriale con options

let rec factOpt n =
    match n with
    | 0 -> Some 1
    | _ when n < 0 -> None
    | _ ->
        let t = factOpt (n - 1)
        match t with
        | Some k -> Some (n * k)
        | None -> None
 
(*

Uso di "when" in un pattern
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Con "when" e' possibile porre delle condizioni su un pattern 

 | p when b1 -> e1

 equivalen a:

    p -> if b then e1 else ... valuta i pattern successivi .....

NOTA
^^^^
Evitare di usare when quando non serve

Esempio:

il pattern     xs when xs = []            si scrive semplicemente     []
il pattern     x :: xs when xs = []       si scrive semplicemente     [x]

o di scriverecondizioni troppo complesse, che l'interprete non riesce
a decidere se sono esaustive e disgiunte

Esempio di PM utile:

match e with
  | (x,y) when x = y -> e1  // se e ha la forma (x,y) con x = y
  | _ -> e2
  
*)

// Esempi di applicazione di factOpt

let n1 = factOpt -2 
// val n1 : int option = None

let n2 = factOpt 4 ;
// val n2 : int option = Some 24


(*

Esercizio 1
^^^^^^^^^^^
                             
Definire la funzione

      printfact : int -> string

che calcola il fattoriale di un intero n e restituisce una stringa che descrive il risultato;
se il fattoriale non e' definito, va restituito un opportuno messaggio.
Per calcolare il fattoriale, usare factOpt.

Per trasformare un intero nella corrispondente stringa, usare la funzione string
(ad esempio, 'string 3' e' la stringa "3", 'string -3' e' la stringa "-3").

Esempi:

printfact 3 ;;
//val it : string = "3! =  6"

printfact -3 ;;
//  it : string = "the factorial of -3 is not defined"

*)

let printfact n =
    let t = factOpt n
    match t with
    | None -> "the factorial of " + (string n) + " is not defined"
    | Some k -> (string n) + "! = " + (string k)


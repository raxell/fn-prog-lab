
// ---- LISTE ------

(* Libro di testo HR, Capitolo 4, in particolare sezioni 1,2,3,4,5   


1. Vediamo come **costruire** liste

2. Vediamo come **usare** una lista

esempio della dualità insita in ogni tipo di dato: introduzione/eliminazione



Una lista e' una sequenza finita di elementi **dello stesso tipo.**



DEFINIZIONE DEL TIPO LIST
^^^^^^^^^^^^^^^^^^^^^^^^^

Sia 'T un tipo qualunque. L'espressione
   
      'T list 
  
denota il tipo di una lista i cui elementi hanno tipo T

========    

Esempi:

             int list  --->  lista di interi 

            char list  --->  lista di caratteri  

  (int * string) list  --->  lista i cui elementi sono coppie int * string
   
    (int -> int) list  --->  lista i cui elementi sono funzioni di tipo int -> int

      (int list) list  --->  lista i cui elementi sono liste di interi
      
             'a  list  --->  lista i cui elementi hanno tipo 'a

 ('a * int * 'a)  list --->  lista i cui elementi sono triple 'a * int * 'a
             

Notare che:

-  Il costruttore list ha precedenza piu' alta di * e ->.

   Quindi:

      int * string list    equivale a      int * (string list)  
                                           // coppie della forma (n : int, ls : string list ) 
                                    
                                              
      int -> float list    equivale a        int -> (float list)
                                             // funzioni da int a float list

- list e' associativo a sinistra

   Quindi

     int list list     equivale a     (int list) list
                                      // liste di liste di int   
                                          


DEFINIZIONE TERMINI DI TIPO T LIST
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Sia T un tipo qualunqe.
I termini di tipo 'T list' sono definiti induttivamente come segue:

(1) [** BASE INDUZIONE **]

   [] e' un termine di tipo 'T list.
   []  rappresenta la lista vuota.

(2) [** PASSO INDUTTIVO **]

    Supponiamo che xs sia un termine di tipo 'T list' e x sia un termine di tipo T. Allora 
   
       x :: xs

    e' un termine di  tipo 'T list'.
    Inoltre, se xs rappresenta la lista contenente gli elementi

      x1 ; ...  ;  xn
      
    il  termine  'x :: xs' rappresenta la lista contenente gli elementi 

            x ; x1 ; ...  ;  xn

    ossia la lista ottenuta  ponendo x in testa alla lista xs.

    Nel termine  x :: xs:

    -  ::    e'  un operatore, chiamato  *cons*.
    -   x    e' detta la  *testa (head)*  della lista
    -   xs   e' detta la   *coda (tail)*  della lista.

   Il termine x :: xs puo' essere rappresentato dall'albero
   
                       :: (cons)
                     /    \             x : T           
                    /      \           xs : T list
                   x       xs  
              (testa)       (coda) 


L'operatore :: (cons) associa a destra:

      x1  ::  x2 :: xs    equivale a       x1  ::  (x2 :: xs) 
        

(In termini matematici una lista è la minima soluzione all'equazione

 'T list = Nil | Cons of ('T * 'T list)

 )


NOTA
^^^^

Dalla definizione, segue che un termine ha tipo 'T list'  SE E SOLO SE
e' costruibile partendo dalla lista vuota
applicando un numero finito di volte il passo induttivo
(ogni volta cons va applicato a termini di tipo T e 'T list').


Esempi:
^^^^^^^^^^^^^^^^

i)    1 :: []     

        ::
       /  \
      1     []

Termine di tipo 'int list' avente testa 1 e coda []      
Rappresenta la lista contente l'elemento 1


ii)    2 ::  1 :: []   

        ::
       /  \
      2    ::
          /  \
         1    []


Lista di tipo 'int list' avente testa 2 e per coda la lista '2::[]'.
Rappresenta la lista contenente gli elementi 2 e 1.


iii)  "due" ::  1 :: []


Questo termine non e' ben tipabile.
Infatti, una espressione della forma

   "due" :: xs

ha senso solo se xs ha tipo 'string list'.
Nel nostro caso, a destra di cons c'e' il termine
   
        1 :: []

che ha tipo 'int list'.

    

SINTASSI ALTERNATIVA
^^^^^^^^^^^^^^^^^^^^

Per rappresentare una lista, anziche' usare l'operatore cons si possono
scrivere gli elementi in essa contenuti fra parentesi quadre, separati da ;

Ad esempio:

     [1]       equivale a      1 :: []
 [2 ; 1]       equivale a      2 :: 1 :: []  

L'uso di cons e'  fondamentale nel pattern matching.


EAGER EVALUATION
^^^^^^^^^^^^^^^^

F# è un longuaggio "strict" ("eager"), come Java, Python, etc...
Questo significa che, prima di costruire la lista, vengono valutati i suoi elementi.

Esempio:


let ls = [ 10 + 5 ; 12 - 2 ; 3 * 4 ] 

L'interprete crea il seguente binding

 val ls : int list = [15; 10; 12]

in cui nella lista compaiono i valori delle espressioni usate nella definizione.

Verificare cosa produce la valutazione di 

let ls = [ 10 + 5 ; 12 / 0 ; 3 * 4 ] 

*)


// Esempi  di liste

let l1  = []     // lista vuota
// val it : 'a list = []

let l2 = 1 :: [] 
// val it : int list = [1]

let l3 = 100 :: 1 :: [] 
//  ìval l1 : int list = [100 ; 1]


let bl = [ true ; false ; true ] 
//val bl : bool list = [true ; false ; true]


let bl1 =  [ 1 < 2;  1 = 2 ;  true ] 
// val bl1 : bool list = [true ; false ; true]
// Notare la eager evaluation (gli elementi sono valutati prima di costruire la lista)

 
let pl = [ ("a",3) ; ("ggg",6) ] 
// val pl : (string * int) list = [("a", 3); ("ggg", 6)]
 

let funl = [ cos ; sin ;  fun x -> x * 3.5 ]  
// val funl : (float -> float) list  - lista di funzioni float -> float

 
let ll= [ [1;2] ; [3;4] ] 
//val ll : int list list - lista di liste di interi
 
// Attenzione a non usare la virgola per separare gli elementi di una lista
//(ricordare che la virgola costruisce una tupla)

let x1l = [ "uno" ; "due" ] ;;
let x2l = [ "uno" , "due" ] ;;
// che differenza c'e' fra xl1 e xl2 ?

(**

ESERCIZIO
^^^^^^^^^

Scrivere qual e' il tipo e il valore dei seguenti termini; verificare la risposta usando l'interprete.


  [ 100 / 2 ; 100 / 2]             [ 100 / 2 , 100 / 2 ]


  [ "100 / 2"  ;  "100 / 2 " ]    [  100 / 2  ;  "100 / 2 " ]  [  100 / 2  ,  "100 / 2 " ]

  [ 100 / 2 ; 100 / 1 ; 100 / 0]   


    
**)


//   --------  RANGE EXPRESSION  ---------

(*  Le range espression sono espressioni computazionali che permettono di costrure liste *)   

// Esempi (per capire cosa sono veramente guardare il libro, capitolo 12, magari a fine corso)

[3 .. 10] ;;    // lista degli interi fra 3 e 10
// val it : int list = [3; 4; 5; 6; 7; 8; 9; 10]

[3 .. 2 .. 20] ;;   // lista degli interi fra 3 e 10 con passo 2
// val it : int list = [3; 5; 7; 9; 11; 13; 15; 17; 19]

[10 .. -1 .. 0] ;;   // passo negativo
// val it : int list = [10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]

['c' .. 'f'];;  // lista dei caratteri fra 'c' e 'f'
// val it : char list = ['c'; 'd'; 'e'; 'f']

(*
Le range expressions sono solo un modo di enumerare qualunque struttura dati che
implementa l'interfacciaIEnumerable -- in pratica questo significa che posso usarle per
sequenze, array, insiemi etc
   *)

let ar= [|'A'..'z'|]
let n = ar.[5]

// inizio e fine possono essere expressioni arbitrariamente complesse

let rr =  [(min 1 0) .. (max 10 20)];;



// ---- Pattern Matching su Liste -----------


(*
OK; ora che ho costruito una lista, chec osa ci faccio? Beh, per cominciare
devo essere in grado di analizzarla, e questo significa: PM!

La struttura standard è la seguente

let f xs =
    match xs with
        [] -> e1
        | y :: ys -> e2

dove e2 probabilmente menziona y,ys
   *)

let str xs =
    match xs with
        [] -> "Empty list"
        | y :: ys -> "head: " + string y + " tail: " + string ys


let s = str [1;2;3]

(* Questo crea un ambiente locale dove y -> 1, ys -> [2;3].

Ricordatevi y:: ys è un patterns (il cui nome è irrilevante, potrebbe essere hd :: tail)

Meglio non scrivere:

let str ys =
    match ys with
        [] -> "Empty list"
        | y :: ys -> "head: " + string y + " tail: " + string ys // a quale ys ci riferiamo?

La regola di lexical scoping ci dice l'ultimo, ma inutile fare confusione              

*)

// Si possono fare PM più articolati:

let rec longer xs =
    match xs with
        [] -> false
        |[x] -> false
        |x:: y::ys  -> true

// scritta meglio con wildcard

let rec longerb xs =
    match xs with
        |x:: y::ys  -> true
        | _ -> false
        
// PM parziale

let tail xs =
    match xs with // nota compiler warning
        _ :: ys -> ys

// e nel caso vuoto? Lo vediamo nella lezione prossima

        
// ---  RICORSIONE SU LISTE ---


(*

La forma generale di una funzione ricorsiva che ha argomento
composto da una lista ls e':

let rec f ... ls ... =
    match ls with
        | [] -> e
        | x::xs -> .... f xs ...

Vengono distinti due casi:

- CASO BASE:

  Se ls e' la lista vuota, la funzione f restituisce il valore di e

- CASO INDUTTIVO:

  Supponiamo che ls abbia la forma x :: xs
  Per calcolare il valore da restituire, si chiama ricorsivamente f  sulla lista xs

Il caso induttivo e' ben fondato in quanto la chiamata ricorsiva e' fatta
sulla lista xs che e' piu' piccola della lista ls di partenza.

(NOTA: il compilatore non controlla la fondatezza della chiamata ricorsiva, perchè, ehm
 è indecidibile)

 Occorre quindi prestare attenzione
che le chiamate ricorsive coinvolgano liste piu' piccole di quella di partenza.

Va inoltre evitato di esplodere inutilmente il pattern matching,
considerando  'casi particolari' che non sono tali.


*)


(*

Esempio 1
^^^^^^^^^

Definire la funzione ricorsiva

   sumlist : int list -> int 

che calcola la somma degli elementi di una lista di interi.


====

Supponiamo che ls sia la lista

   ls = [ x0 ; x1 ; x2 ; ... ; xn ]

La ricorsione si basa sul fatto che

    x0 + x1 + x2 + .... + xn  =  x0  +  (x1 + x2 + .... + xn)

Per calcolare

   x1 + x2 + .... + xn

posso chiamare ricorsivamente sumlist sulla sottolista

  [ x1 ; x2 ; ... ; xn ]  // coda di ls


Quindi, data una lista ls  avente testa x0 e coda xs, vale

  
   sumlist  ls =  x + sumlist xs      (#)

La ricorsione e' usata in modo fondato perche' la lista xs e' piu' piccola di ls.


Il caso base e' la definizione di


   sumlist []

Che valore deve avere 'sumlist []' ?

Supponiamo che ls contenga solo l'elemento x

   ls = [x]

ls ha testa  x e coda [].
La definizione induttiva

   sumlist  ls =  x + sumlist xs 

ponendo ls = [x], si riscrive come:

   sumlist [x] = x  + sumlist []

D'altra parte, sappiamo che  'sumlist [x]' deve valere x.
Segue che

    sumlist [] = 0

In altri termini, 'sumlist []' e' l'elemento neutro della somma.    


*)  


let rec sumlist ls =  
    match ls with 
        | [] -> 0    
        | x0:: xs  -> x0 + sumlist xs  
 
sumlist [1 .. 10] ;;   // 55

(*

La definizione di prodlist e' analoga.


*)



let rec prodlist ls =  
    match ls with 
        | [] -> 1    
        | x0:: xs  -> x0 * prodlist xs  
 
prodlist [1 .. 5 ] ;;  // 120


 
(*

Esempio 2
^^^^^^^^^

Definire la funzione ricorsiva

  sumprod : int list -> int * int
  
che data una list ls restituisce la coppia (sum,prod),
dove sum e' la somma degli elementi della lista
e prod e' il prodotto degli elementi della lista.

Facciamolo in una unica passata


*)  

let rec sumprod ls =  
    match ls with 
        | [] -> (0,1) 
        | x0::xs ->  
            let (sum,prod) = sumprod xs // (##)
            ( x0 + sum, x0 * prod) 
/// (##) Notare l'uso del pattern (sum,prod) per estrarre le componenti del valore di  sumprod xs 

sumprod [1..10]  // (55, 3628800)

 
// Non vi sorprenderà che esiste una ricchissima libreria
// (parzialmente decritta nel capitolo 5.1 del libro) e referenziata a
// https://msdn.microsoft.com/visualfsharpdocs/conceptual/collections.list-module-%5bfsharp%5d

// per una discussione approfondita: https://fsharpforfunandprofit.com/posts/list-module-functions/

   
(*

Esempio 3
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

     length : 'a list -> int

che calcola la lunghezza di una lista, definita dalla ricorsione:

len [] = 0
len x :: xs = 1 + òen xs

*)
 

let rec length ls =  
    match ls with 
    | [] -> 0 
    | _::xs -> 1 + length xs 
 
length [3..7] ;;   // 5
length ['a'..'z'] ;;  // 26

(*

Notare che anche la soluzione

let rec len ls =  
    match ls with 
    | [] -> 0
    | [x] -> 1
    | x::xs -> 1 + len xs 

e' corretta, ma e' un brutto uso del pattern matching,
in quanto non c'e' nessun motivo per trattare a parte il caso [x] (lista con un solo elemento)
che è colto dal patter x :: xs con xs = []

Soluzioni di questo tipo, in cui si aggiungono casi  ridondanti,  vanno evitate e verranno
penalizzate negli esami

*)   

 
(*

Esempio 4
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

     append : 'a list * 'a list -> 'a list

che concatena due liste. Ecco la ricorsione


[] @ ys = ys
(x :: xs) @ ys = x :: (xs @ ys)
                          
NOTA
^^^^^
La funzione append e' gia' implementata in F# dall'operatore @,
che e' un operatore infisso con associativita' a destra.

Esempio

 [1 ; 2 ] @ [ 3 ; 4 ] =  [ 1 ; 2 ; 3 ; 4 ]

 [1 ; 2 ] @ [ 3 ; 4 ] @ [5]  =  [ 1 ; 2 ; 3 ; 4 ; 5 ]


*) 
 
let rec append (xs, ys) = 
    match xs with 
    | [] -> ys 
    | z::zs -> z :: append (zs, ys) 
 
 
append ( [ 1 ; 2] , [ 3 ; 4 ; 5 ] ) ;; //  [1; 2; 3; 4; 5]

(*

NOTA SU  ::  e   @
^^^^^^^^^^^^^^^^^^

Attenzione a usare correttamente gli operatori :: (cons) e @ (append)

  ::     ha  operandi di tipo T e 'T list'
   @     ha entrambi gli operandi 'T list'.

Per inserire un elemento x : T  in testa a una lista xs : T list si puo' scrivere

   x  :: xs

Non scrivete [x] @ xs, che è inefficiente (richiede la chiamat di una funzione
                                           (x ::  []) @ xs, invece di un costruttore


Per inserire un  elemento x : T  in coda a una lista standard si puo' solo usare  @

   xs @  [x]

mentre   xs :: x   non ha senso (errore di tipo).

Vedremo come si possono definire strutture dati più complesse con accessi sia in testa che in coda.
Ma se dovete sempre accedere alla coda, forse la lista non è la struttura dati giusta
*)   



(*


Esempio 5
^^^^^^^^^

Definire la funzione ricorsiva (polimorfa)

   rev : 'a list -> 'a list

che inverte gli elementi di una lista (analoga a List.rev):

  rev [ x0 ; x1 ; .... ; x(n-1) ; xn ]  = [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

===

Notare che il pattern matching permette di estrarre il primo elemento della lista
ma non l'ultimo.

La lista 
 
     [ xn ; x(n-1) ; ... ; x1 ; x0 ]  

puo' pero' essere vista come la concatenazione delle due liste

   [ xn ; x(n-1) ; ... ; x1 ]      [x0]

Inoltre [ xn ; x(n-1) ; ... ; x1 ] puo' essere costruita con la chiamata ricorsiva

  rev [ x1 ; .... ; x(n-1) ; xn ]   // l'argomento di rev e' la coda di ls
  
rev [] = []
rev x :: xs = rev xs @ [x]

Questa implentazione è sub-ottimale O(|xs|^2), mentre potremmo essere lineare.
Vedremo come ottenere questo usando accumulatori e/o continuazioni
*)

let rec rev ls =  
    match ls with 
    | [] -> [] 
    | x :: xs -> rev xs @ [x]  
 

rev [1 .. 10] ;;        // [10; 9; 8; 7; 6; 5; 4; 3; 2; 1]

(* POLIMORFISMO

Tutte queste funzioni sono applicabili qualunque sia il contenuto della lista:
dopo tutto la lunghezza di una lista, la sua reverse, sono operazioni strutturali,
al contrario della sumlist che prevede che la lista contenga interi.

Questa proprietà si chiama **polimorfismo**, è sata introdotta da
Robin Milner nel 1978 in "A Theory of Type Polymorphism in
Programming" in SML ed è arrivata in linguaggi imperativi quasi 30
anni dopo (generics in Java 5 e C# 2.0, ca. 2004).


Un  *tipo polimorfo (polymorphic type)* e' un tipo parametrico su
 *variabili di tipo (type variable)* . Logicamente corrisponde
 a un quantificatore universale:

 'a list significa forall a : type. 'a list

 ==> list è quindi una funzione che prende un tipo e ne ritorna un altro

 e quindi gode della regola di instanziazione (che è poi applicazione funzionale
                                               a livello di tipo):

    'a list         t is a type
    -----------------------------
           t list  is a type


Ora generalizzate:

     'a t           s type
     ---------------------
           s t type                           

           

In genere le variabili di tipo sono denotate da lettere greche;
nel codice  si usano gli apici davanti al nome, esempio:

   'a  , 'b,  ....


Una  funzione (piu' in generale, una espressione) e' detta *polimorfa*
se ha tipo polimorfo.

Per ragioni di decidibilità della type inference, ci si limita al cosiddetto polimorfismo **prenesso**:
le variabili sono tutte quantificate all'esterno:

SI:  f : forall 'a. 'a list -> 'a list
NO:  f : forall 'a. 'a list -> (forall 'b. 'a * 'b)

*) 

// Esempi di funzioni polimorfe

// La regina, l'identità:

let id x = x

let n3 = id 3

let ff = id (fun x -> x * x)

// swap: data la coppia (x,y) restituisce (y,x)

let swap (x,y) = (y,x)


(*

La funzione swap e' polimorfa in quanto puo' essere applicata ad arbitrarie coppie (x,y)

Il tipo della funzione e'

     swap : 'a * 'b -> 'b * 'a

che e' il tipo di una funzione che, data una coppia di tipo  'a * 'b,
restituisce una coppia di tipo 'b * 'a.

Quando swap e' applicata, le variabili di tipo 'a e 'b sono istanziate in base agli argomenti

*)   


let s1 = swap ("uno", 2) 
// val s1 : int * string = (2, "uno")


let s2 = swap ( "funzione  successore" , fun x -> x + 1 ) 
// val s2 : (int -> int) * string = (<fun:s2@181>, "funzione  successore")



// Definire la funzione first che restituisce la prima componente di una coppia

let first (x,y) = x

// val first : x:'a * y:'b -> 'a


// altra definizione

let first1 p =
  let (x,y) = p  
  x
// val first1 : 'a * 'b -> 'a


// con PM
let first2 p =
    match p  with
        (x,y) ->   x

(*
NON confondete polimorfismo con eredetarietà e overloading

   *)

let fp x =  2 * x //val ff : (int -> int)

let ffl x = 2. * x   //  ffl : float -> float
   
// sono due funzioni distinte, dove il compilatore permette soltanto
// di usare lo stesso simbolo per due operazioni distinte


// ---    TYPE CONSTRAINTS ------------

(** 
Detto questo, F# offre una combinazione di polimorfismo ed ereditarietà --
      la cosa è fatta molto più elegantemente in Haskell con il concetto di type class.

Questo per gestire in modo generale la nozione di uguaglianza e ordine:

(=)  : 'T -> 'T -> bool when 'T : equality
(<)  : 'T -> 'T -> bool when 'T : comparison

etc

Sono operazioni polimorfe, ma non ogni tipo di dato ammette uguaglianza e ordinamento
(che sono definite, almeno nella parte funzionale, strutturalmente, es (a,b) = (c,d) sse a = b & c = d).

Ad esempio, due funzioni non possono essere comparate per uguaglianza -- e come
potrebbero? Sono oggetti infiniti
    

*)

// cosa succede qui?

let g1 = fun x -> x + 1
let g2 = fun x -> 1 + x

// let b = (g1 = g2)

// o qualcosa di più pratico

open System


let form1 = new System.Windows.Forms.Form()


let form2 = new System.Windows.Forms.Form()

let b =
    form1 = form2

// ma...
// let b1 =     form1 <= form2
(*

Le funzioni polimorfe possono richiedere vincoli aggiuntivi sulle variabili di tipo.
Nel seguito incontreremo due tipi di vincoli su una variabile di tipo 'a:
   

1)   when 'a : equality

Sul tipo che istanzia 'a deve essere  definita l'uguaglianza =


2)  when 'a : comparison 
 
Sul tipo che istanzia 'a deve essere  definito l'ordinamento <


Quindi, quando definisco funzioni che usano uguaglianza, ordinamento, eredito questi vincoli.

E' una buona cosa!
**)   


// 1) Esempio di funzione  polimorfa con vincolo su = 

let  cmp (x,y) =
    if x = y then "uguali" else "diversi" 

//   cmp : x:'a * y;'a -> string when 'a : equality
//                               ^^^^^^^^^^^^^^^^^^

//  Esempi di chiamate corrette (il tipo 'a e' istanziato correttamente) 

cmp ( 1, 10 - 9 )   //  "uguali"     'a = int

cmp ( "sole", "luna" )   // "diversi"    'a = string

cmp ( (0,0) , (0,1) )  // "diversi"     'a = int * int



// Verificare come viene valutata l'espressione
// cmp (fun x -> x + 1, fun x -> x + 1)      

// 2) Esempio di  funzione polimorfa  con vincolo su < 

let  less (x,y) =
    if x < y then "minore" else "maggiore o uguale" 

(*

Il tipo della funzione e'  

   less : x:'a * y:'a -> string when 'a : comparison
                                ^^^^^^^^^^^^^^^^^^^^

In questo caso 'a puo' essere istanziato con un qualunque tipo
su cui sia definito l'ordinamento < , ad esempio int, char, bool, ..

  
*)


less('z' , 'a')    // "maggiore o uguale"    'a = char

less("abaco" , "baco" )   // "minore"  'a = string
// < sul tipo string corrisponde all'ordinamento lessicografico

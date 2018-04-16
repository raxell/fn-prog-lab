(*
*** Libro di testo [HR]: Cap 9 ****

*)   

 (***  FUNZIONI ITERATIVE   ***)


(*

In genere la ricorsione non consente un uso ottimale della memoria
in quanto ogni chiamata ricorsiva richiede l'allocazione di memoria nello stack
per memorizzare i dati utilizzati nella computazione (ambiente locale).

In programmazione imperativa il problema si risolve riscrivendo una funzione
ricorsiva in forma iterativa.

In programmazione funzionale la ricorsione non puo' essere completamente eliminata
(non esistono i cicli ...).
Si puo' pero' scrivere una funzione ricorsiva in modo tale che si comporti di fatto
come una iterazione, e quindi permetta un uso ottimale delle risorse di memoria.


Esempio: fattoriale
^^^^^^^^^^^^^^^^^^^

Consideriamo la funzione fattoriale fact

*)  

// fact : int -> int
let rec fact n =
  match n with 
    | 0  -> 1                   // n = 0 
    | _  -> n * fact (n-1)      // n > 0 


(*

Esempio di computazione della funzione fact
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

fact 4   =  4 * ( fact 3 ) 
         =  4 * ( 3 *  ( fact 2 ) )
         =  4 * ( 3 *  ( 2 * ( fact 1 ) )) 
         =  4 * ( 3 *  ( 2 * ( 1 * (fact 0) ) ))
         =  4 * ( 3 *  ( 2 * ( 1 * 1 ) ))

Notare che:

- vengono effettuate 4 chiamate ricorsive, ossia:

    'fact 3'  'fact 2'   'fact 1'   'fact 0'

- ogni chiamata ricorsiva richiede la definizione di un nuovo ambiente locale
  in cui viene memorizzato il legame (binding) per n.

Infatti:

* nella chiamata iniziale 'fact 4' si ha  n --> 4   

* nella chiamata 'fact 3' si ha   n --> 3 

e cosi' via.

Gli ambienti locali creati nelle chiamate ricorsive sono mantenuti in uno stack.


Problema
^^^^^^^^

Nel calcolo di

  fact n

il prodotto

   n * fact (n-1)

puo' essere calcolato solamente *dopo* che la computazione di  'fact (n-1)' e' terminata.
Quindi l'ambiente locale che definisce n va mantenuto nello stack fino al termine
della computazione di 'fact (n-1)'.

Nel calcolo di 'fact 4' a un certo punto lo stack deve contenere tutti i binding locali
necessari alla computazione:

   ------------- 
   |  n --> 0  | 
   -------------
   |  n --> 1  |
   -------------
   |  n --> 2  |
   -------------
   |  n --> 3  |   
   -------------
   |  n --> 4  |  
   -------------


Si puo' gestire meglio l'uso della memoria?

L'idea e' di simulare la  versione iterativa del fattoriale
usando un accumulatore.


Versione iterativa del fattoriale
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Nella versione iterativa si sfrutta un accumulatore acc
che viene utilizzato per calcolare gradualmente  il prodotto

   1 * 2 * 3 * .... * n

Piu' precisamente, il calcolo di  n! in modo iterativo
corrisponde alla esecuzione del seguente ciclo:

    ---------------------------------
    |** pseudo codice imperativo **  |    
    |   // calcolo n!                |
    |     acc = 1   // accumulatore  |
    |       k = n                    |
    |   while(k > 0){                |
    |     acc = k * acc              |
    |       k = k - 1                |
    |  }                             |
    |//Quando il ciclo termina vale: |
    |//  acc = n!                    |
    ----------------------------------

Il ciclo viene eseguito n volte.
Al termine del ciclo si ha
 
 acc =  1 *  2 *  ...  (n-2) *  (n-1) * n
              
ossia, acc = n!.


Esempio
^^^^^^^

Supponiamo di voler calcolare 4!. All'inizio si ha

acc |  1
----------
 k  |  4

Dopo la prima iterazione

acc |  4   // = 4 * 1 
---------------------
  k |  3

Dopo la seconda iterazione

acc |  12  // = 3  * 4 =  3 * 4 * 1
------------------------------------
 k  |   2

Dopo la terza iterazione

acc |  24  //  =  2 * 12 = 2 * 3 * 4 * 1    
-------------------------------------------
 k  |   1

Dopo la quarta iterazione

acc |  24   //  =  1 * 24  =  1 * 2 * 3 * 4 * 1 
------------------------------------------------
 k  |   0

e il ciclo termina.

Il valore di acc corrisonde a 4!

*****

Si puo' dimostrare in maniera formale che, quando il ciclo termina.
acc e' uguale a n!.

Si osserva che, al termine di ogni iterazione del ciclo, 
vale questa proprieta' invariante:

(INV)     acc  =  (k + 1 ) * (k + 2 ) * ... * n
               // parte finale del calcolo di n!

Quando il ciclo termina, k vale 0.
Dalla proprieta' (INV), sostituendo k con 0 segue che

      acc  =  (0 + 1 ) * (0 + 2 ) * ... * n 
           =   1 * 2 * ... * n    
           =   n! 

Questo dimostra che, al termine del ciclo, acc vale n!.


Versione iterativa del fattoriale in F#
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Mostriamo ora come si possa simulare la computazione iterativa del fattoriale
e i benefici che si ottengono in termini di utilizzo della memoria.

Definiamo la funzione ausiliaria con accumulatore

    factA : int * int -> int

con l'idea che  la chiamata

    factA (k,acc)

corrisponde all'esecuzione del ciclo del programma iterativo scritto sopra.
I valore di k e acc nell'argomento di factA corrispondono
alle omonime variabili usate nel ciclo definito sopra.

 (i)  Se k = 0, il ciclo  termina e acc corrisponde a n!
      Quindi, la funzione restituisce acc.

(ii)  Se k > 0 , occorre fare una nuova iterazione del  ciclo dove:
      *   il nuovo valore di  k    e'    k-1 
      *   il nuovo valore di acc   e'    k * acc
     Questo corrisponde a eseguira una chiamata ricorsiva
     con argomento (k-1 , k * acc).

Segue che:

 factA(0,acc)  =  acc  // (i)
 factA(k,acc)  =  factA(k-1 , k * acc)  se k > 0  // (ii)

Per calcolare n!, devo eseguire il ciclo con valori iniziali  k=n e acc=1.
Quindi:

  n! = factA(n,1)   

Si puo' dimostrare, per induzione su k, che:
   
   factA(k,acc) =  k!  * acc   

Quindi:

  factA(n,1) =  n! * 1  = n!


Esempio 
^^^^^^^

Per calcolare 4! occorre calcolare

factA(4,1)

Infatti:

factA(4,1)  =  factA(3,4)       //  4 = 4 * 1
            =  factA(2,12)      // 12 = 3 * 4 = 3 * (4 * 1 )
            =  factA(1,24)      // 24 = 2 * 12 = 2 * (3 * (4 * 1))
            =  factA(0,24)      // 24 = 1 * 24 = 1 *  (2 * (3 * (4 * 1)))
            =  24  

Quindi 4! = 24

Possiamo ora definire la funzione  itFact (fattoriale iterativo)


*)

// fattoriale iterativo

// iFact : int -> int
//let  itFact n = 
 

(*

Differenza fra le due versioni della funzione fattoriale
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Versione ricorsiva

Per calcolare 'fact n'  occorre calcolare
 
     n * fact (n - 1)  

La moltiplicazione non puo' essere eseguita subito,
ma va ritardata fino a quando il valore di  fact (n - 1) e' stato calcolato.
Quindi, lo stack deve accumulare *tutti* i binding per n definiti nelle chiamate ricorsive.


-  Versione iterativo

Per calcolare 'itFact_aux k acc' occorre calcolare

         itFact_aux  (k-1)  k*acc  

Dopo la chiamata ricorsiva non c'e' piu' alcuna operazione da compiere,
quindi *non* e' necessario  conservare l'ambiente corrente (binding per k e acc).

Tail recursion
^^^^^^^^^^^^^^

Quando una chiamata ricorsiva viene effettuata
come ultima operazione si parla di *ricorsione in coda* (*tail recursion*).

Una funzione ricorsiva in cui tutte le chiamate ricorsive sono in coda
(ad es., la funzione itFact) e' detta *iterativa*.

L'interprete e' in grado di riconoscere quando una funzione e' ricorsiva in coda;
se questo avviene, la funzione e' compilata in modo che l'uso delle
risorse sia ottimizzato.

Come per la ricorsione, non esiste un'unica ricetta per definire 
funzioni iterative, ogni esempio ha le sue particolarita' (vedi esercizi). 

*)   

// ESEMPIO:  FOLD

(*

Un esempio notevole di funzione iterativa e' la funzione  *fold (List.fold)*.

Infatti, la definizione ricorsiva di fold e':


let rec fold f e xs  =
  match xs with
    | [] -> e
    | y :: ys -> fold f (f e y) ys 
// fold : f:('a -> 'b -> 'a) -> e:'a -> xs:'b list -> 'a

Al contrario, foldBack  (List.foldBack) non e' iterativa:


let rec foldBack f xs e =
  match xs with
    | [] -> e
    | y :: ys -> f y (foldBack f ys e)
                      ^^^^^^^^^^^^^^^ 
                        chiamata  *NON* in coda! 
                          
// foldBack : f:('a -> 'b -> 'b) -> xs:'a list -> e:'b -> 'b

Quindi:
- fold e' efficiente (iterativa), foldBack no
- nei casi in cui sia possibile usare sia fold che foldBack
(es., somma elementi di una lista),  scegliere fold.


*)   


// ESEMPIO: REVERSE LIST

// versione ricorsiva
// rev : a list -> 'a list
let rec rev ls =
  match ls with
    | [] -> []
    | x :: xs -> rev xs @ [x]
(*
Esempio di computazione
^^^^^^^^^^^^^^^^^^^^^^^

rev [1 ; 2 ; 3] =
  rev [2;3] @ [1] =          
  (rev [3] @ [2]) @ [1] =    
  ((rev [] @ [3]) @ [2]) @[1] =  
  (( []  @ [3]) @ [2]) @[1] =   
  ([3] @ [2]) @[1] =   
  [3;2] @ [1] =      
  [3;2;1]  
    

Analisi complessita'
^^^^^^^^^^^^^^^^^^^

- Per calcolare

rev [x1; x2 ; .... ; xn ]

sono necessario n applicazioni di @.
Partendo dalla lista vuota, ogni applicazione di @ aggiunge
un elemento alla lista che si sta costruendo.
Le n applicazioni di @ sono (vedere esempio sopra):

1)  []  @ [x(n)]  che produce [x(n)]
2)  [x(n)] @ [x(n-1)] che produce [x(n) ; x(n-1)]
3)  [x(n) ; x(n-1)] @ [x(n-2)]  che produce  [x(n) ; x(n-1); x(n-2)]
4)  [x(n) ; x(n-1) ; x(n-2)] @ [x(n-3)] ....
....
n) [x(n) ; x(n-1) ; x(n-3) ... x(n-1) ] @ [x1] che produce la lista invertita

Se xs contiene k elementi la complessita'  di

  xs @ ys

e' proporzionake a k in quanto  sono richiesti k cons per aggiungere gli elementi di xs a ys.
Segue che la complessita' di 

  rev [x1; x2 ; .... ; xn ]

e'  O(n^2) (quadratica).

Infatti:
- sono richieste n applicazioni di @
- al passo k, @ e' applicata a una lista di lunghezza k-1.
Quindi la complessita' e'

0 + 1 + 2 + .... + (n-1) = (n-1) * n /2

che e' quadratica in n

*) 

// versione iterativa

(* definisco una funzione ausiliaria con accumulatore, in cui viene costruita gradualmente
la lista invertita.
*)

// itRev : a list -> 'a list
//let itRev ls =

(*

Analisi complessita'
^^^^^^^^^^^^^^^^^^^

La funzione e' tail-recursive, quindi non richiede allocazione di stack.
Ad ogni chiamata ricorsiva di

  itRev_aux xs acc

il primo elemento di xs e' inserito in testa ad acc.
Quindi, ogni chiamata richiede tempo costante (indipendente dalla lunghezza di xs).

Per invertire una lista di lunghezza n sono richieste n chiamate ricorsive,
quindi la complessita'n e' O(n) (lineare).

Riassumendo:
^^^^^^^^^^^

- rev      ha complessita' quadratica
- itRev    ha complessita' lineare

La differenza si puo' verificare sperimentalmente eseguendo le due funzioni
con liste grosse.
Per avere piu' dettagli sull'uso delle risorse, attivare nell'interprete:

#time
   
*)

#time



// BIG LIST
(*

Definire la funzione ricorsiva

    bigList : int -> int list

che costruisce una lista contenente n volte 1.
Definire quindi la corrispondente versione iterativa

   itBigList : int -> int list
  
Verificare sperimentalmente le prestazioni delle due funzioni con n grandi.

*)

// bigList : n:int -> int list
let rec bigList n =
  match n with
    | 0 -> []
    | _ -> 1 :: bigList (n - 1)  // n > 0

// itBigList : int -> int list
// let itBigList n =
 


(* Lo schema descritto sopra (funzione iterativa con  un unico accumulatore),
non e' abbastanza generale da poter essere applicato a tutte le funzioni ricorsive.

Ad esempio, consideriamo la funzione count che conta in nodi di un albero.

*)   

type binTree<'a> =
  | Null
  | Node of 'a * binTree<'a> *   binTree<'a>

// definizione ricorsiva di count 
//    count : BinTree<'a> -> int
let rec count = function
    | Null          -> 0
    | Node(_,left,right) -> count left + count right + 1

let t1 = Node (2, Null, Node ( 4 , Null, Null ) ) 
let t2 = Node(10,t1,t1)
count t1  // 2
count t2  // 5


// funzione count definita mediante accomulatore
let countAcc tree =
  let rec countAcc_aux tr acc =
    match tr with
      | Null -> acc
      | Node(_,left,right) -> countAcc_aux left (countAcc_aux right (1 + acc))
  countAcc_aux tree 0

countAcc t1  // 2
countAcc t2  // 5


(*

La funzione countAcc_aux *NON* e' ricorsiva in coda.
Infatti,  la chiamata ricorsiva piu' interna

  countAcc_aux right (1 + acc)

non e' ricorsive in coda (non e' ultima operazione della funzione).

Per poter ottenere una definizione tail recursive della funzione count
si puo' utilizza una tecnica piu' generale nota come *Continuation Passing Style (CPS)*

*)


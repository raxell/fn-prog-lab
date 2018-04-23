// using the dll

//#r "listFS.dll"
#r "treeFS.dll"
open IFSet

// let's create a set
let oneset = ofList [1..10]

// what? I can't see squat!

// utility to see what's going on, but still return a set
let print ss =
  let xs = toList ss  |> List.sort // to re-sort after operations
  printf "result set: %A\n" xs
  ss

let _ = print oneset

let ss = add 3 empty |> add 5 |> add 2 |> add 77 |> print

let ss1 = (add 3 ss) |> print;;

let u = union oneset ss |> print;;


// let's test our Set implementation wrt the Set collection
#r "FsCheck"
open FsCheck

(*
 - a suite of tests, as a list, each with a label to identify the culprit

- note that use of List as a mediation betweeb FSet and Set
 + we cannot generate FSet directly, since the representatio type is hidden

- note that use of List.sort to avoid false postives due to FSet as list  not being ordered

*)


let test_set x (xs : int list) ys =
  [
    empty |> toList = (Set.empty |> Set.toList) |@ "empty set"
    isEmpty (ofList xs) = Set.isEmpty (Set.ofList xs) |@ "is empty"
    contains x (ofList xs) = Set.contains x (Set.ofList xs) |@ "contains"
    (add x (ofList xs) |> toList |> List.sort) = (Set.add x (Set.ofList xs) |> Set.toList)  |@ "add"
    (union  (ofList xs) (ofList ys) |> toList |> List.sort) =
       (Set.union  (Set.ofList xs)  (Set.ofList ys)|> Set.toList) |@ "union"
    (ofList xs |> toList |> List.sort ) = (Set.ofList xs |> Set.toList) |@ "list"
    (ofList xs |> count) = (Set.ofList xs |> Set.count) |@ "count"
    ]

do Check.Quick test_set


(* Let's actually use this ADT for something useful: evaluation of
logical formulae, where propositional variables takes values in an
enviroment 'env', with the understanding that if a variable occurs in env, then
it's true, otherwise false. Example

V 2 true in [1..10]

V 22 false in [1..10]
*)

type form =
  V of int
  | K of bool
  | Not of form
  | And of form * form


type env = IFSet

let rec eval env = function
  K b -> b
  | V i -> contains i env
  | Not f -> eval env f |> not
  | And (f1, f2) -> (eval  env f1) && (eval env f2)


let test() =
    let env = Gen.sample 20 20 Arb.generate<int>  // generate an env with at most 20 elements
    match Gen.sample 30 1 Arb.generate<form> with // generate a formula with size at most 30
        [e] -> printfn "\tinput formula: %A\n\tinput env: %A\n\tresult of evaluation: %A" e env ( eval (IFSet.ofList env) e)
        |_ -> failwith "does not happen"

// we'll see how to define better generators later
    
  

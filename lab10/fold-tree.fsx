#r "FsCheck"
open FsCheck

// are HO functions just over lists? Absolutely no, they work on any algebraic data type

// Standard presentation of binary trees
type 'a binTree =
    | Leaf    // empty tree
    | Node of 'a  * 'a binTree * 'a binTree ;;   // Node(root, left, right)

let t1 = Node (2, Leaf, Leaf)
let t2 = Node (2, Leaf, Node ( 4 , Leaf, Leaf ) );;
let t7 = Node (7, Leaf, Node (10, Leaf, Node ( 13 , Leaf, Leaf ))) ;; 
let t8 = Node ( 8, Node ( 11, Leaf, Leaf), Leaf ) ;; 
let t5 = Node ( 5, t7, t8 ) ;;

// a function on trees
let rec intToFloatTree btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            Node ( float r , intToFloatTree left , intToFloatTree right ) ;;

let t2r = intToFloatTree t2

// can we make it HO? 
let rec mapTree f btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
             Node ( f r, mapTree f left, mapTree f right )
//                 ^^^^^

let t2rH = mapTree float t2

let prop_float (ts : int binTree) =
   intToFloatTree ts =  mapTree float ts
do Check.Quick prop_float 



// we can extend other HO combinators to trees (and to any other user-defined datatypes)
// Remember search?
let rec search  (x, btree) =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) ||  search  (x,left)  ||  search (x,right)  ;;
//           ^^^^^^^

// It's an instance of the exists combinator on trees

let rec exTree  p btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            p r ||  exTree p left  ||  exTree p right  ;;
//         ^^^^ 

let prop_exS  x (ts : int binTree) =
   search(x,ts) = exTree (fun n -> x = n) ts
do Check.Quick prop_exS  


// what about filter? Ah, that's a little harder:
// It's easy to filter to a list, say inorder here

let rec filterToList  pred btree =
    match btree with
        | Leaf -> []
        | Node ( r, left, right ) ->
            let fleft  =  filterToList pred left
            let fright =  filterToList pred right
            if pred r then fleft @ [r] @  fright else  fleft  @  fright

let l = filterToList (fun x -> x % 2 = 0) t5

// But how do you  that preserves the structure of the tree?
// Use options!

let rec filterTreeOpt  pred btree =
    match btree with
        | Leaf -> Leaf
        | Node ( r, left, right ) ->
            let fleft =   filterTreeOpt pred left
            let fright =  filterTreeOpt pred right
            if pred r then Node(Some r,fleft, fright)
            else  Node(None,fleft,fright)

// if pred r does not hold put a None: the tree has the same shape.


let nt5o = t5, filterTreeOpt (fun x -> x > 8) t5

// towards fold

//summing the values in a int binTree

let rec sum   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> r + sum left + sum right

// number of nodes
let rec count   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> 1 + count left + count right

// depth of a tree
let rec depth   btree =
    match btree with
        | Leaf -> 0
        | Node ( r, left, right ) -> 1 + (max (depth left) ( depth right))

(*
Those functions follow the same pattern and should be implementable
 with a fold-like combinator:

 Remember lists

*)

// (('a -> 'b -> 'b) -> 'a list -> 'b -> 'b)
let rec foldBack f_cons xs f_nil =
   match xs with
     | [] -> f_nil 
     | x::xs' -> f_cons x (foldBack f_cons xs' f_nil)

// a complicated way to do nothing

let idlist xs =
  let f_cons = (fun y ys -> y :: ys)
  let f_nil = []
  foldBack  f_cons xs f_nil 


// a FOLDBACK-like combinator  for trees
let rec fold_tree f_node f_leaf tree = 
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) ->
        let fleft = fold_tree f_node f_leaf left
        let fright = fold_tree f_node f_leaf right
        f_node x  fleft fright
// f_node:('a -> 'b -> 'b -> 'b) -> f_leaf:'b -> tree:'a binTree -> 'b


// same but with recursion singled out

let rec fold_tree2 f_node f_leaf tree =
    let recurse = fold_tree2 f_node f_leaf
    match tree with
        | Leaf -> f_leaf
        | Node (x, left, right) ->
            f_node x  (recurse left) (recurse right)        

// sum of elements
let sumbtf = fold_tree (fun x l r -> x + l + r) 0 

// # of elements
let countf ts = fold_tree (fun _ l r -> 1 + l + r) 0 ts

let c = countf t2

// depth of tree
let depthtf ts = fold_tree (fun _ l r ->  1 + (max l  r))  0 ts

let d = depthtf t2

// inorder traversal

let inOrder tr = fold_tree (fun x l r -> l @ [x] @ r) []  tr



// defining filterOpt
let filterTreeOptF p tr =
  fold_tree (fun x l r -> if p x then Node(Some x,l,r) else Node(None,l,r) ) Leaf tr 

// --------------------------



(*

Rules for creating catamorphisms
We saw above that creating a catamorphism is a mechanical process:

Create a function parameter to handle each case in the structure.
For non-recursive cases, pass the function parameter all the data associated with that case.
For recursive cases, perform two steps:
First, call the catamorphism recursively on the nested value.
Then pass the handler all the data associated with that case, but with the result of the catamorphism replacing the original nested value.


*)

// Another version of trees with leafs conining data

type 'a tree =
    | Tip of 'a    // empty tree
    | Branch of  'a tree * 'a tree ;;   // Node(root, left, right)

// the fold-like recursion schema

let rec ftree f_br f_tip tree =
    let recourse = ftree f_br f_tip 
    match tree with
        | Tip x -> f_tip x
        | Branch (left, right) ->
            f_br (recourse left) (recourse right)

// val ftree : f_br:('a -> 'a -> 'a) -> f_tip:('b -> 'a) -> tree:'b tree -> 'a  let sumbtf = fold_tree (fun x l r -> x + l + r) 0 

// # of elements
let countf2 ts = ftree (fun l r -> l + r) (fun _ -> 1) ts


// depth of tree
let depthtf2 ts = ftree (fun l r ->  1 + (max l  r))  (fun _ -> 0) ts
  
// I can play this game with every algebraic datatype

#r "FsCheck"
open FsCheck

type 'a binTree =
    | Null
    | Node of 'a  * 'a binTree * 'a binTree

let t2 = Node(2, Null, Node(4, Null, Null))
let t7 = Node(7, Null, Node(10, Null, Node(13, Null, Null)))
let t8 = Node(8, Node(11, Null, Null), Null)
let t5 = Node(5, t7, t8 )
let t9 = Node(9, Null, Node (12, Null, Null))
let t6 = Node(6, t9, Null)
let t3 = Node(3, t5, t6)
let t1 = Node(1, t2, t3)

// 1.1
let rec intToFloatTree bTree =
    match bTree with
    | Node(x, left, right) -> Node(float x, intToFloatTree left, intToFloatTree right)
    | Null -> Null

// 1.2
let rec inorderToList bTree =
    match bTree with
    | Null -> []
    | Node(x, left, right) -> (inorderToList left) @ x :: (inorderToList right)

let rec preorderToList bTree =
    match bTree with
    | Null -> []
    | Node(x, left, right) -> x :: (inorderToList left) @ (inorderToList right)

let prop_sameElements (t1: int binTree) =
    (inorderToList t1 |> List.sort) = (preorderToList t1 |> List.sort)

do Check.Quick prop_sameElements
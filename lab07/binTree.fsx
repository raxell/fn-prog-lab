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

// 1.3
let rec search (x, bTree) =
    match bTree with
    | Null -> false
    | Node(y, left, right) -> x = y || search (x, left) || search (x, right)

let rec contains (x, xs) =
    match xs with
    | [] -> false
    | hd :: tl -> hd = x || contains (x, tl)

let prop_search (x, bTree) =
    search (x, bTree) = contains (x, inorderToList bTree)

do Check.Quick prop_search

// 1.4
let rec filterToList (f, bTree) =
    match bTree with
    | Null -> []
    | Node(x, left, right) ->
        match f x with
        | true -> filterToList (f, left) @ x :: filterToList (f, right)
        | _ -> filterToList (f, left) @ filterToList (f, right)

let isEven x = x % 2 = 0
let isSmall x = x < 5

// 1.5
let rec count bTree =
    match bTree with
    | Null -> (0, 0)
    | Node(x, left, right) ->
        let (lNodes, lLeaves) = count left
        let (rNodes, rLeaves) = count right
        match left = Null && right = Null with
        | true -> (1 + lNodes + rNodes, 1 + lLeaves + rLeaves)
        | _ -> (1 + lNodes + rNodes, lLeaves + rLeaves)

// 1.6
let rec depthToList (n, bTree) =
    match (n, bTree) with
    | (_, Null) -> []
    | (0, Node(x, _, _)) -> [x]
    | (_, Node(_, l, r)) -> depthToList (n - 1, l) @ depthToList (n - 1, r)

// 1.7
type direction = L | R

let rec runPath (path,  bTree) =
    match (path, bTree) with
    | ([], Node(x, _, _)) -> Some x
    | (hd :: tl, Node(_, l, _)) when hd = L -> runPath (tl, l)
    | (hd :: tl, Node(_, _, r)) when hd = R -> runPath (tl, r)
    | _ -> None

// 2.1 i)
let rec insert (x, bTree) =
    match bTree with
    | Null -> Node(x, Null, Null)
    | Node(y, l, r) when x < y -> Node(y, insert (x, l), r)
    | Node(y, l, r) when x > y -> Node(y, l, insert (x, r))
    | _ -> bTree

// 2.1 ii)
let rec insertFromList (ls, bTree) =
    match ls with
    | [] -> bTree
    | hd :: tl -> insertFromList (tl, insert (hd, bTree))

// 2.1 iii)
let intList = [20; 10; 60; 15; 40; 100; 30; 50; 70; 35; 42; 58; 75; 32; 37]
let strList1 = ["pesca"; "banana"; "uva"; "albicocca"; "nocciola"; "ribes"]
let strList2 = ["limone"; "ciliegia"; "mela"; "pera"; "noce"]

let intTree = insertFromList (intList, Null)
let strTree1 = insertFromList (strList1, Null)
let strTree2 = insertFromList (strList2, strTree1)

// 2.2
let rec search1 (x, bTree) =
    match bTree with
    | Node(y, _, _) when x = y -> true
    | Node(y, l, _) when x < y -> search1 (x, l)
    | Node(y, _, r) when x > y -> search1 (x, r)
    | _ -> false

// 2.3
let rec min bTree =
    match bTree with
    | Node(x, l, _) when l = Null -> Some x
    | Node(x, l, _) when l <> Null -> min l
    | _ -> None

// 2.4
let rec subtree (x, bTree) =
    match bTree with
    | Node(y, _, _) when x = y -> bTree
    | Node(y, l, _) when x < y -> subtree (x, l)
    | Node(y, _, r) when x > y -> subtree (x, r)
    | _ -> Null

// 2.5
let rec searchPath (x, bTree) =
    match bTree with
    | Node(y, _, _) when x = y -> [y]
    | Node(y, l, _) when x < y && searchPath (x, l) <> [] -> y :: searchPath (x, l)
    | Node(y, _, r) when x > y && searchPath (x, r) <> [] -> y :: searchPath (x, r)
    | _ -> []


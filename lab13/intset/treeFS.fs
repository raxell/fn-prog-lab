module  IFSet

// we use binary search trees as an alternative implementation of sets

type IFSet = Leaf | Node of int * IFSet * IFSet



let empty =   Leaf

let isEmpty t =
    (t = Leaf)

let rec contains  x  btree =
    match btree with
        |  Leaf -> false
        |  Node (r, left, right) ->
            ( x = r ) ||
            ( (x < r) && contains  x left ) ||
            ( not (x < r) && contains  x right );;  // do we need the guard not (x > r) ?

let rec add  x  btree  =
    match btree with
        | Leaf -> Node(x, Leaf, Leaf)
        | Node(r, left, right) when x = r ->  btree
        | Node(r, left, right) when x < r ->  Node(r,  (add  x left) , right )
        | Node(r, left, right)  ->  Node(r , left, (add x  right) )



//
let rec fold_treeBack f_node tree f_leaf  =
  match tree with
    | Leaf -> f_leaf
    | Node (x, left, right) -> f_node x (fold_treeBack f_node left f_leaf ) (fold_treeBack f_node right f_leaf )

let rec union  s1 s2 =
    match s1 with
     | Leaf -> s2
     | Node(x,ltr,rtr) -> let ts = add x s2
                          let tsl = union  ltr ts
                          union  rtr tsl;;


let ofList list = List.fold (fun t x -> add x t ) Leaf list

// preorder here
let toList tree  = fold_treeBack (fun x l r -> x :: l @ r) tree []





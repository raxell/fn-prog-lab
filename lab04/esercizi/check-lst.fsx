#r "FsCheck"
#r "proflist"
open FsCheck
#load "lst.fsx"

// positive integers up to n
let posIntGen n = Arb.filter (fun x -> x >= 0 && x <= n) Arb.from<int>

let prop (xs : int list) (ys : int list) =
    Prop.forAll (posIntGen 1000) <| fun x ->
    [Proflist.rmEven xs = Lst.rmEven xs |@ "RM_EVEN"
     Proflist.rmOddPos xs = Lst.rmOddPos xs |@ "RM_ODDPS"
     Proflist.split xs = Lst.split xs |@ "SPLIT"
     Proflist.cmpLength (xs, ys) = Lst.cmpLength (xs, ys) |@ "COMPARE"
     Proflist.remove(x, xs) = Lst.remove(x, xs) |@ "REMOVE"
     Proflist.removeDup(xs) = Lst.removeDup(xs) |@ "REMOVEDUP"
     Proflist.downto0 x = Lst.downto0 x |@ "DOWN"
     Proflist.upto x = Lst.upto x |@ "UPTO"
         ]


do printf "validating all your functions ... "; Check.Quick prop

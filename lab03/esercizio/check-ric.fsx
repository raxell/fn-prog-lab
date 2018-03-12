#r "FsCheck"
#r "prof"
open FsCheck
#load "ric.fsx"


let prop1 m n  =
    Prof.gcd (m, n) = Ric.gcd(m,n)

let prop2 (PositiveInt n) (NonNegativeInt m) =
    Prof.simplify (m, n) = Ric.simplify(m,n)


let prop3 (NonNegativeInt m) =
    Prof.sum1 m = Ric.sum1 m


let prop4 (NonNegativeInt n) (NonNegativeInt m) =
    n > m ==> lazy (Prof.sum2 (m, n) = Ric.sum2(m,n))


let prop5(PositiveInt n) ( NormalFloat m) =
    Prof.fexp (m, n) = Ric.fexp(m,n)


let farb = Arb.fromGen (Gen.elements [0..15] )
    
let prop6 =
    Prop.forAll farb (fun m ->
    
    Prof.fib m = Ric.fib m    )
    
do printf "Validating gcd ... "; Check.Quick prop1
do printf "Validating simplify ";  Check.Quick prop2
do printf "Validating sum1 ... ";  Check.Quick prop3
do printf "Validating sum2 ... ";  Check.Quick prop4
do printf "Validating fexp ... ";  Check.Verbose prop5
do printf "Validating fib ... ";  Check.Quick prop6
// 

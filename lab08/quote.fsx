
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type aexp =
  | C of int
  | Sum of aexp * aexp
  | Prod of aexp * aexp
  | Abs of aexp


let rec eval t  =
    match t with
    | C n         -> n
    | Abs t      -> eval t  |> abs
    | Sum(t1,t2)  -> eval t1  + eval t2 
    | Prod(t1,t2) -> eval t1  * eval t2

    
//  Generates aexps
let rec fsharpToaexp code  =
    match code with
    | Int32(n)  -> C n

    | SpecificCall <@ (+) @> (_, _, [lhs; rhs]) ->
        let e1 = fsharpToaexp lhs 
        let e2 = fsharpToaexp rhs 
        Sum(e1,e2)

  
    | SpecificCall <@ ( *) @> (_, _, [lhs; rhs]) ->
        let e1 = fsharpToaexp lhs 
        let e2 = fsharpToaexp rhs 
        Prod(e1,e2)

    | SpecificCall <@ (abs) @> (_, _, [lhs]) ->
        fsharpToaexp lhs |> Abs

    | expr -> failwithf "Unknown Expr:\n%A" expr

let e = fsharpToaexp <@ abs (2 * 3 + (-4) )  @>
let r =
    fsharpToaexp <@ (2 *  3 * 4 * 9 + 2 ) @>
    |> eval

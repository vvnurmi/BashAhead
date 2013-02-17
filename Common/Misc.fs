module BashAhead.Common.Misc

let random = System.Random()
let chooseOne (list : 'a list) = list.[random.Next list.Length]
let rec isConstant = function
    | a :: (b :: c as tail) -> a = b && isConstant tail
    | _ :: [] | [] -> true
let rec transpose = function
    | (_ :: _) :: _ as x -> List.map List.head x :: transpose (List.map List.tail x)
    | _ -> []
let capitalize = function
    | "" -> ""
    | s -> s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)
let tryFindStart key map =
    let startsWith (k : string) = k.StartsWith(key, System.StringComparison.InvariantCultureIgnoreCase)
    match List.filter (fst >> startsWith) map with
    | [ k, v ] -> Some v
    | _ -> None
    
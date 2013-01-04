module Misc

let rec isConstant = function
    | a :: (b :: c as tail) -> a = b && isConstant tail
    | _ :: [] | [] -> true
let rec transpose = function
    | (_ :: _) :: _ as x -> List.map List.head x :: transpose (List.map List.tail x)
    | _ -> []
let capitalize = function
    | "" -> ""
    | s -> s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)

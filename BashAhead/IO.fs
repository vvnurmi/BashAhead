module IO

open System

type Color = ConsoleColor

let getCommand () =
    Console.ReadLine().PadRight(1).Substring(0, 1).ToLowerInvariant()
let promptUser () =
    getCommand () |> ignore
let printfc str color =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printf "%s" str
    Console.ForegroundColor <- oldColor
let rec print = function
    | (s, c) :: tail ->
        printfc (s + "\t") c
        print tail
    | [] ->
        printfn ""

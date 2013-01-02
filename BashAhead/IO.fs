module IO

open System

type Color = ConsoleColor
type FormatElement =
    | StrColor of string * Color
    | Str of string

let getCommand () =
    Console.ReadLine().PadRight(1).Substring(0, 1).ToLowerInvariant()
let promptUser () =
    getCommand () |> ignore
let printfc str color =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printf "%s" str
    Console.ForegroundColor <- oldColor
let rec print =
    let printCore s c =
        printfc (s + "\t") c
    function
    | StrColor(s, c) :: tail ->
        printCore s c
        print tail
    | Str(s) :: tail ->
        printCore s Color.Gray
        print tail
    | [] ->
        printfn ""

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
let printCore str color =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printf "%s\t" str
    Console.ForegroundColor <- oldColor
let rec print = function
    | StrColor(s, c) :: tail ->
        printCore s c
        print tail
    | Str(s) :: tail ->
        printCore s Color.Gray
        print tail
    | [] ->
        printfn ""

module IO

open System

type Color = ConsoleColor
type FormatElement =
    | Row of FormatElement list
    | StrColor of string * Color
    | Str of string

let getCommand () =
    Console.ReadLine().PadRight(1).Substring(0, 1).ToLowerInvariant()
let promptUser () =
    getCommand () |> ignore
let printCore str color =
    let oldColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    printf "%s" str
    Console.ForegroundColor <- oldColor
let rec print = function
    | Row(cells) ->
        List.iter (fun cell -> print cell; printf "\t") cells
        printfn ""
    | StrColor(s, c) ->
        printCore s c
    | Str(s) ->
        printCore s Color.Gray

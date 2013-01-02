module IO

open System

type Color = ConsoleColor
type FormatElement =
    | Table of TableElement list
    | StrColor of string * Color
    | Str of string
and TableElement =
    | Row of FormatElement list
type FormatContext = { x : int; y : int; color : Color }
type FormattedString = { context : FormatContext; str : string }

// Helpers
let setContext c =
    Console.CursorLeft <- c.x
    Console.CursorTop <- c.y
    Console.ForegroundColor <- c.color
let getCellSizes = function
    | Row(cells) :: _ -> List.map (fun cell -> 10) cells // TODO
    | [] -> []
let rec format context = function
    | Table(rows) ->
        formatTable context (getCellSizes rows) rows
    | StrColor(s, c) ->
        [ { context = { context with color = c }; str = s } ]
    | Str(s) ->
        [ { context = context; str = s } ]
and formatTable context cellSizes = function
    | Row(cells) :: tail ->
        let rec formatCells context = function
            | (cell, size) :: tail ->
                let newContext = { context with x = context.x + size }
                (format context cell) @ formatCells newContext tail
            | [] -> []
        let fmts = formatCells context (List.zip cells cellSizes)
        let newContext = { context with y = context.y + 1 }
        fmts @ formatTable newContext cellSizes tail
    | [] -> []

// Public interface
let getCommand () =
    Console.ReadLine().PadRight(1).Substring(0, 1).ToLowerInvariant()
let promptUser () =
    getCommand () |> ignore
let rec print context elem =
    let printCore fmt =
        setContext fmt.context
        printf "%s" fmt.str
    let fmts = format context elem
    List.iter printCore fmts
    setContext context
let printPrompt = print { x = 0; y = 20; color = Color.White }
let printStatus = print { x = 0; y = 0; color = Color.Gray }
let clear = Console.Clear

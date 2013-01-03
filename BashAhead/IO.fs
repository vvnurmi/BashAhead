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
let rec transpose = function
    | (_ :: _) :: _ as x -> (List.map List.head x) :: (transpose (List.map List.tail x))
    | _ -> []
let setContext c =
    Console.CursorLeft <- c.x
    Console.CursorTop <- c.y
    Console.ForegroundColor <- c.color
let rec getSize = function
    | Table(rows) ->
        let (cellWidths, cellHeights) = getTableCellSizes rows
        (List.sum cellWidths, List.sum cellHeights)
    | StrColor(s, _) | Str(s) -> (s.Length, 1)
and getTableCellSizes rows =
    let rowSizes = List.map (fun (Row(cells)) -> List.map getSize cells) rows
    let maxCellWidths = rowSizes |> transpose |> List.map (List.map fst >> List.max)
    let maxCellHeights = rowSizes |> List.map (List.map snd >> List.max)
    (maxCellWidths, maxCellHeights)
let rec format context = function
    | Table(rows) as t ->
        let (cellWidths, cellHeights) = getTableCellSizes rows
        formatTable context cellWidths (List.zip rows cellHeights)
    | StrColor(s, c) -> [ { context = { context with color = c }; str = s } ]
    | Str(s) -> [ { context = context; str = s } ]
and formatTable context cellWidths = function
    | (Row(cells), cellHeight) :: tail ->
        let rec formatCells context = function
            | (cell, size) :: tail ->
                let newContext = { context with x = context.x + size }
                (format context cell) @ formatCells newContext tail
            | [] -> []
        let fmts = formatCells context (List.zip cells cellWidths)
        let newContext = { context with y = context.y + cellHeight }
        fmts @ formatTable newContext cellWidths tail
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

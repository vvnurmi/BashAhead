module BashAhead.Common.ConsoleIO

open System
open Misc
open Types
open State
open Conditions

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
let rec getSize = function
    | Table(rows) ->
        let (cellWidths, cellHeights) = getTableCellSizes rows
        List.sum cellWidths, List.sum cellHeights
    | StrColor(s, _) | Str s -> s.Length, 1
and getTableCellSizes rows =
    let rowSizes = List.map (fun (Row cells) -> List.map getSize cells) rows
    if not << isConstant <| List.map List.length rowSizes then failwith "Table rows have different numbers of cells"
    let maxCellWidths = rowSizes |> transpose |> List.map (List.map fst >> List.max)
    let maxCellHeights = rowSizes |> List.map (List.map snd >> List.max)
    List.map ((+) 2) maxCellWidths, maxCellHeights
let rec format context = function
    | Table(rows) as t ->
        let cellWidths, cellHeights = getTableCellSizes rows
        formatTable context cellWidths <| List.zip rows cellHeights
    | StrColor(s, c) -> [ { context = { context with color = c }; str = s } ]
    | Str s -> [ { context = context; str = s } ]
and formatTable context cellWidths = function
    | (Row cells, cellHeight) :: tail ->
        let rec formatCells context = function
            | (cell, size) :: tail ->
                let newContext = { context with x = context.x + size }
                format context cell @ formatCells newContext tail
            | [] -> []
        let fmts = formatCells context <| List.zip cells cellWidths
        let newContext = { context with y = context.y + cellHeight }
        fmts @ formatTable newContext cellWidths tail
    | [] -> []

// Public interface
let rec print context elem =
    let printCore fmt =
        setContext fmt.context
        printf "%s" fmt.str
    let fmts = format context elem
    List.iter printCore fmts
    setContext context
let clear = Console.Clear
let clearArea c width height =
    let row = Row [ Str <| new String(' ', width) ]
    let format = Table <| List.replicate height row
    print c format
let getCommand promptFmt =
    let c = { x = 0; y = 20; color = Color.White }
    print c promptFmt
    let width, height = getSize promptFmt
    print { c with x = c.x + width + 1; y = c.y + height - 1 } <| Str ""
    let input = Console.ReadLine()
    clearArea c (width + input.Length + 1) height
    input
let checkGameOver =
    rState {
        let! gameOver = getGameOver
        if gameOver then Str "Game over." |> getCommand |> ignore
        return not gameOver
    }
let rec getUserInput promptFormat validateInput =
    rState {
        let input = getCommand promptFormat
        match validateInput input with
        | Some x -> return x
        | None -> return! getUserInput promptFormat validateInput
    }
let rec getUserCommand getCommands getName testPrecondition formatCommand getParamValues =
    rState {
        let! commands = getCommands
        let! commandOks = adapt2 List.map testPrecondition commands
        let! promptFormat = adapt3 List.map2 formatCommand commands commandOks
        let! okCommands = adapt2 List.filter testPrecondition commands
        let! names = adapt2 List.map getName okCommands
        let validateInput str = tryFindStart str <| List.zip names okCommands
        let! command = Table promptFormat |> getUserInput <| validateInput
        let! paramValues = getParamValues command
        match paramValues with
        | Some values ->
            let names = List.map fst values
            let promptFormat = Table <| List.map (fun v -> Row <| [ Str <| v ]) names
            let validateInput str = tryFindStart str values
            let! param = getUserInput promptFormat validateInput
            return command, [ param ]
        | None -> return command, []
    }

let formatCreature c =
    let nameElem = Str <| (String.replicate c.distance " ") + c.name
    let hpElem =
        match health c.maxHitpoints c.hitpoints with
        | Brilliant -> StrColor("Brilliant", Color.White)
        | Ok -> StrColor("Ok", Color.Green)
        | Bruised -> StrColor("Bruised", Color.Cyan)
        | Wounded -> StrColor("Wounded", Color.Yellow)
        | Critical -> StrColor("Critical", Color.Red)
        | Dead -> StrColor("Dead", Color.DarkGray)
    let weaponStr = if c.weaponKnown then c.weaponName else "???"
    let propertyElem = Str(sprintf "[%s]" weaponStr)
    Row [ nameElem; hpElem; propertyElem ]
let formatMessages messages =
    List.map (fun m -> Row [ Str m ]) <| List.rev messages

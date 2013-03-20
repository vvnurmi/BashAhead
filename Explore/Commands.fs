module BashAhead.Explore.Commands

open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
open Types
open State

type Command =
    | Fight
    | Move
    | Common of BashAhead.Common.Commands.Command

let commands =
    let commonCommands = List.map (fun c -> Common c) BashAhead.Common.Commands.commonCommands
    Fight :: (Move :: commonCommands)
let getName command =
    match command with
    | Fight -> "Pick a fight"
    | Move -> "Go to..."
    | Common c -> BashAhead.Common.Commands.getName c
let formatCommand command =
    Row [ StrColor(getName command, Color.White) ]
let getParamValues command =
    rState {
        let! locations = getLocations
        match command with
        | Move ->
            let items = Some <| List.map (fun l -> l.name, l.id) locations
            let promptFormat = Table <| List.map (fun l -> Row [ Str l.name ]) locations
            return items, promptFormat
        | _ -> return None, Str ""
    }
let execute command parameters =
    rState {
        match command, parameters with
        | Fight, [] -> return [ ToBattle ]
        | Move, [ id ] -> return [ HeroMoves id ]
        | Common c, p ->
            let commonEvents = BashAhead.Common.Commands.execute c p
            return List.map (fun e -> Event.Common e) commonEvents
        | _ ->
            failwith <| sprintf "Invalid command %O" command
            return []
    }

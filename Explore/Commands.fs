module BashAhead.Explore.Commands

open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
open Types
open State

type Command =
    | Fight
    | Move
    | Common of BashAhead.Common.Commands.Command

let getCommands =
    rState {
        let! locations = getLocations
        let commonCommands = List.map (fun c -> Common c) BashAhead.Common.Commands.commonCommands
        return Fight :: (Move :: commonCommands)
    }
let getName command =
    rState {
        match command with
        | Fight -> return "Pick a fight"
        | Move -> return "Go to..."
        | Common c -> return BashAhead.Common.Commands.getName c
    }
let testPrecondition command =
    rState {
        match command with
        | Fight -> return true
        | Move -> return true
        | Common _ -> return true
    }
let formatCommand command active =
    assert active
    rState {
        let! name = getName command
        return Row [ StrColor(name, Color.White); Str "" ]
    }
let getParamValues command =
    rState {
        let! locations = getLocations
        match command with
        | Move -> return Some <| List.map (fun l -> l.name, l.id) locations
        | _ -> return None
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

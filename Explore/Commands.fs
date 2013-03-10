module BashAhead.Explore.Commands

open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
open Types
open State

type Command =
    | Fight
    | Move of LocationId
    | Common of BashAhead.Common.Commands.Command

let getCommands =
    rState {
        let! locations = getLocations
        let moveCommands = List.map (fun l -> Move l.id) locations
        let commonCommands = List.map (fun c -> Common c) BashAhead.Common.Commands.commonCommands
        return Fight :: moveCommands @ commonCommands
    }
let getName command =
    rState {
        match command with
        | Fight -> return "Pick a fight"
        | Move id ->
            let! location = getLocation id
            return sprintf "Go to the %s" location.name
        | Common c -> return BashAhead.Common.Commands.getName c
    }
let testPrecondition command =
    rState {
        match command with
        | Fight -> return true
        | Move _ -> return true
        | Common _ -> return true
    }
let formatCommand command active =
    assert active
    rState {
        let! name = getName command
        return Row [ StrColor(name, Color.White); Str "" ]
    }
let execute command =
    rState {
        match command with
        | Fight -> return [ ToBattle ]
        | Move id -> return [ HeroMoves id ]
        | Common c ->
            let commonEvents = BashAhead.Common.Commands.execute c
            return List.map (fun e -> Event.Common e) commonEvents
    }

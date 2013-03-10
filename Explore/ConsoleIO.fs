﻿module BashAhead.Explore.ConsoleIO

open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
open State
open Commands
open Types
open Update

let printStatus = print { x = 0; y = 0; color = Color.Gray }
let printLocation = print { x = 0; y = 9; color = Color.White }
let printMessages = print { x = 0; y = 10; color = Color.Gray }

let formatLocation =
    rState {
        let! location = getHeroLocation
        let locStr =
            match location with
            | Some loc -> sprintf "You are at the %s." loc.name
            | None -> "You are in a limbo!"
        return Row <| [ StrColor(locStr, Color.White) ]
    }
let showState =
    rState {
        let! heroRow = liftCommon <| lift formatCreature getHero
        let! locationRow = formatLocation
        let! messageRows = liftCommon <| lift formatMessages getMessages
        printStatus <| Table [ heroRow ]
        printLocation <| Table [ locationRow ]
        printMessages <| Table messageRows
    }

// Returns false if quit was requested
let processUI () =
    rwState {
        let! userEvents = getUserEvents getCommands getName testPrecondition formatCommand execute
        do! adapt2 List.iter applyEvent userEvents
    }

module BashAhead.Battle.ConsoleIO

open BashAhead.Common.ConsoleIO
open BashAhead.Common.Misc
open BashAhead.Common.State
open AI
open Commands
open State
open Types
open Update

let printStatus = print { x = 0; y = 0; color = Color.Gray }
let printAIState = print { x = 0; y = 9; color = Color.White }
let printMessages = print { x = 0; y = 10; color = Color.Gray }

let formatAIState =
    rState {
        let! state = getAIState
        match state with
        | AllIdle -> return "The monsters stand idly."
        | AllAttack -> return "The monsters charge to attack!"
        | OneAttack mId ->
            let! m = getMonster mId
            return capitalize <| sprintf "%s has challenged you!" m.name
        | AllFlee -> return "The monsters flee in panic!"
        | AllSurrender -> return "The monsters have surrendered to your mercy."
    }
let showState =
    rState {
        let! heroRow = lift formatCreature (liftCommon getHero)
        let! monsterRows = lift (List.map formatCreature) getMonsters
        let! aiStateRow = formatAIState
        let! messageRows = lift formatMessages (liftCommon getMessages)
        printStatus <| Table(heroRow :: monsterRows)
        printAIState <| Str aiStateRow
        printMessages <| Table messageRows
    }
let rec getUserEvents () =
    rState {
        // getCommands testPrecondition formatCommand execute
        let! commands = getCommands
        let! commandOks = adapt2 List.map testPrecondition commands
        let! promptFmt = adapt3 List.map2 formatCommand commands commandOks
        let command = getCommand <| Table promptFmt
        let! okCommands = adapt2 List.filter testPrecondition commands
        match tryFindStart command <| List.map (fun c -> getName c, c) okCommands with
        | Some c -> return! execute c
        | None -> return! getUserEvents ()
    }
// Returns false if quit was requested
let processUI () =
    rwState {
        clear ()
        do! showState
        do! liftCommon clearMessages
        let! ok = liftCommon checkGameOver
        if ok then
            let! userEvents = getUserEvents ()
            if not <| List.exists ((=) Quit) userEvents then
                do! applyEvents userEvents
                do! getAIEvents %|> applyEvents
                do! getGameEvents %|> applyEvents
                return true
            else return false
        else return false
    }

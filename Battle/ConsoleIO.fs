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
let processUI () =
    rwState {
        let namesAndCommands = List.map (fun c -> getName c, c) commands
        let! tableRows = adapt2 List.map formatCommand commands
        let question = Row [ StrColor("Choose your action!", Color.Cyan); Str "" ]
        let promptFormat = Table <| question :: tableRows
        let! command, parameters = getUserCommand namesAndCommands promptFormat getParamValues
        let! userEvents = execute command parameters
        do! applyEvents userEvents
        do! getAIEvents %|> applyEvents
        do! getGameEvents %|> applyEvents
    }
let rec uiLoop () =
    rwState {
        clear ()
        do! showState
        do! liftCommon clearMessages
        let! battleOver = getBattleOver
        if not battleOver then
            do! processUI ()
            do! uiLoop ()
    }

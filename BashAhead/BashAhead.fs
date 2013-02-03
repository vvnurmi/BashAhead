module BashAhead.BA

[<assembly:System.Runtime.InteropServices.ComVisible false>]
[<assembly:System.Reflection.AssemblyVersion "1.0.*">]
[<assembly:System.CLSCompliant true>]
do ()

open Misc
open ConsoleIO
open Types
open State
open Conditions
open Commands
open Update
open AI

let state = stateUnit

let createHero =
    rwState {
        let! id = getNewId
        return {
            id = id
            name = "Hero"
            maxHitpoints = 42<hp>
            hitpoints = 42<hp>
            weaponName = "sword"
            weaponKnown = true
            distance = 0
        }
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
let formatAIState =
    rState {
        let! state = getAIState
        match state with
        | AllIdle -> return "The monsters stand idly."
        | AllAttack -> return "The monsters charge to attack!"
        | OneAttack mId ->
            let! m = getCreature mId
            return capitalize <| sprintf "%s has challenged you!" m.name
        | AllFlee -> return "The monsters flee in panic!"
        | AllSurrender -> return "The monsters have surrendered to your mercy."
    }
let showState =
    rState {
        let! heroRow = lift formatCreature getHero
        let! monsterRows = lift (List.map formatCreature) getMonsters
        let! aiStateRow = formatAIState
        let! messageRows = lift formatMessages getMessages
        printStatus <| Table(heroRow :: monsterRows)
        printAIState <| Str aiStateRow
        printMessages <| Table messageRows
    }
let rec getUserActions () =
    rState {
        let! commands = getCommands
        let! commandOks = adapt2 List.map testPrecondition commands
        let! promptFmt = adapt3 List.map2 formatCommand commands commandOks
        let command = getCommand <| Table promptFmt
        let! okCommands = adapt2 List.filter testPrecondition commands
        match tryFindStart command <| List.map (fun c -> getName c, c) okCommands with
        | Some c -> return! execute c
        | None -> return! getUserActions ()
    }
let checkGameOver =
    rState {
        let! gameOver = getGameOver
        if gameOver then Str "Game over." |> getCommand |> ignore
        return not gameOver
    }
let rec uiLoop () =
    rwState {
        clear ()
        do! showState
        do! clearMessages
        let! ok = checkGameOver
        if ok then
            let! userActions = getUserActions ()
            if not <| List.exists ((=) Action.Quit) userActions then
                do! applyActions userActions
                do! getAIChanges %|> applyChanges
                do! getGameActions %|> applyActions
                do! uiLoop ()
    }
let main =
    rwState {
        let! hero = createHero
        do! setHero hero
        do! uiLoop ()
    }
let _, finalState = rwState.RunOp(main, State.stateUnit)

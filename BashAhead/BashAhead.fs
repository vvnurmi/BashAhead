module BA

open Misc
open IO
open Types
open State
open Conditions
open Commands
open Update
open AI

let state = stateUnit
let random = System.Random()
let chooseOne (list : string []) = list.[random.Next list.Length]

let createMonster =
    rwState {
        let! id = getNewId
        return {
            id = id
            name = chooseOne [| "orc"; "goblin"; "wolf" |]
            maxhitpoints = 12<hp>
            hitpoints = 12<hp>
            weaponName = chooseOne <| Array.map fst (Map.toArray Library.weapons)
            weaponKnown = false
            distance = 10
        }
    }
let createHero =
    rwState {
        let! id = getNewId
        return {
            id = id
            name = "Hero"
            maxhitpoints = 42<hp>
            hitpoints = 42<hp>
            weaponName = "sword"
            weaponKnown = true
            distance = 0
        }
    }

let formatCreature c =
    let nameElem = Str <| (String.replicate c.distance " ") + c.name
    let hpElem =
        match health c.maxhitpoints c.hitpoints with
        | Brilliant -> StrColor("Brilliant", Color.White)
        | Ok -> StrColor("Ok", Color.Green)
        | Wounded -> StrColor("Wounded", Color.Yellow)
        | Critical -> StrColor("Critical", Color.Red)
        | Dead -> StrColor("Dead", Color.DarkGray)
    let weaponStr = if c.weaponKnown then c.weaponName else "???"
    let propertyElem = Str(sprintf "[%s]" weaponStr)
    Row [ nameElem; hpElem; propertyElem ]
let formatMessages messages =
    List.map (fun m -> Row [ Str m ]) <| List.rev messages
let showState =
    rState {
        let! heroRow = lift formatCreature getHero
        let! monsterRows = lift (List.map formatCreature) getMonsters
        let! messageRows = lift formatMessages getMessages
        printStatus <| Table(heroRow :: monsterRows)
        printMessages <| Table messageRows
    }
let rec getUserActions () =
    rState {
        let commands = getCommands
        let! commandOks = adapt2 List.map testPrecondition commands
        let promptFmt = Table <| List.map2 formatCommand commands commandOks
        let command = getCommand promptFmt
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
                let! gameActions = getGameActions
                let! changes = applyActions <| userActions @ gameActions
                let! aiChanges = AI.getAIChanges
                do! applyChanges <| changes @ aiChanges
                do! uiLoop ()
    }
let main =
    rwState {
        let! hero = createHero
        do! setHero hero
        for i = 1 to 3 do
            let! monster = createMonster
            do! addMonster monster
        do! uiLoop ()
    }
let _, finalState = rwState.RunOp(main, State.stateUnit)

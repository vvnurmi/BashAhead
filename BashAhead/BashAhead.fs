﻿module BA

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
    stateM {
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
    stateM {
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
    stateM {
        let! heroRow = lift formatCreature getHero
        let! monsterRows = lift (List.map formatCreature) getMonsters
        let! messageRows = lift formatMessages getMessages
        printStatus <| Table(heroRow :: monsterRows)
        printMessages <| Table messageRows
        do! clearMessages
    }
let rec getUserActions () =
    stateM {
        let commands = getCommands
        let formatter = fun op -> Table <| List.map (formatCommand op) commands
        let! promptFmt = adapt formatter testPrecondition
        let command = getCommand promptFmt
        let! okCommands = adapt (fun op -> List.filter op commands) testPrecondition
        match tryFindStart command <| List.map (fun c -> getName c, c) okCommands with
        | Some c -> return! execute c
        | None -> return! getUserActions ()
    }
let checkGameOver =
    stateM {
        let! gameOver = getGameOver
        if gameOver then Str "Game over." |> getCommand |> ignore
        return not gameOver
    }
let rec uiLoop () =
    stateM {
        clear ()
        do! showState
        let! ok = checkGameOver
        if ok then
            let! userActions = getUserActions ()
            if not <| List.exists (fun a -> a = Action.Quit) userActions then
                do! updateAI
                let! gameActions = getGameActions
                do! updateState <| userActions @ gameActions
                do! uiLoop ()
    }
let main =
    stateM {
        let! hero = createHero
        do! setHero hero
        for i = 1 to 3 do
            let! monster = createMonster
            do! addMonster monster
        do! uiLoop ()
    }
let (_, finalState) = run main State.stateUnit

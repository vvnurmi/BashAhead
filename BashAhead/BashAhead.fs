﻿module BA

open IO
open Types
open State
open Update

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
        }
    }

let formatCreature c =
    let nameElem = Str(c.name)
    let hpElem =
        match c.hitpoints with
        | x when x > c.maxhitpoints -> StrColor("Brilliant", Color.White)
        | x when x = c.maxhitpoints -> StrColor("Ok", Color.Green)
        | x when x > c.maxhitpoints / 2 -> StrColor("Wounded", Color.Yellow)
        | x when x > 0<hp> -> StrColor("Critical", Color.Red)
        | _ -> StrColor("Dead", Color.DarkGray)
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
let getUserActions () =
    stateM {
        printPrompt <| StrColor("Thrust, Swing, Quit? ", Color.White)
        let command = getCommand ()
        return!
            match command with
            | "t" -> attackWeakest
            | "s" -> attackAll
            | "q" -> ret [ Quit ]
            | _ -> ret []
    }
let rec uiLoop () =
    stateM {
        clear ()
        do! showState
        let! gameOver = getGameOver
        if gameOver then
            printPrompt <| StrColor("Game over.", Color.White)
            promptUser ()
        else
            let! userActions = getUserActions ()
            if not <| List.exists (fun a -> a = Quit) userActions then
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

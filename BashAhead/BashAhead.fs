﻿module BA

open System
open Types
open State
open Update

let state = stateUnit
let random = System.Random()
let chooseOne (list : string []) = list.[random.Next(list.Length)]

let createMonster =
    stateM {
        let! id = getNewId
        return {
            id = id;
            name = chooseOne [| "orc"; "goblin"; "wolf" |];
            maxhitpoints = 12<hp>;
            hitpoints = 12<hp>;
            weaponName = chooseOne (Array.map (fun (name, w) -> name) (Map.toArray Library.weapons));
        }
    }
let createHero =
    stateM {
        let! id = getNewId
        return {
            id = id;
            name = "Hero";
            maxhitpoints = 42<hp>;
            hitpoints = 42<hp>;
            weaponName = "sword";
        }
    }

let showCreature c =
    printf "%s\t" c.name
    let printfc str color =
        let oldColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        printf str
        Console.ForegroundColor <- oldColor
    let showHitpoints hp hpMax =
        match hp with
        | x when x > hpMax -> printfc "Brilliant\t" ConsoleColor.White
        | x when x = hpMax -> printfc "Ok\t" ConsoleColor.Green
        | x when x > hpMax / 2 -> printfc "Wounded\t" ConsoleColor.Yellow
        | x when x > 0<hp> -> printfc "Critical\t" ConsoleColor.Red
        | _ -> printfc "Dead\t" ConsoleColor.DarkGray
    let showProperties weapon =
        printf "[%s]\t" weapon
    showHitpoints c.hitpoints c.maxhitpoints
    showProperties c.weaponName
    printfn ""

let showState =
    stateM {
        do! lift showCreature getHero
        let! monsters = getMonsters
        for m in monsters do showCreature m
    }
let getUserActions () =
    stateM {
        printfn "Thrust, Swing, Quit?"
        let command = Console.ReadLine().PadRight(1).Substring(0, 1).ToLowerInvariant()
        return!
            match command with
            | "t" -> attackWeakest
            | "s" -> attackAll
            | "q" -> ret [ Quit ]
            | _ -> ret []
    }
let rec frameStep actions =
    stateM {
        do! updateState actions
        let! gameOver = getGameOver
        if gameOver <> null then
            printfn "Game over. %s" gameOver
            Console.ReadLine () |> ignore
            return ()
        else
            return! uiLoop ()
    }
and uiLoop () =
    stateM {
        do! showState
        let! userActions = getUserActions ()
        if not (List.exists (fun a -> a = Quit) userActions) then
            let! gameActions = getGameActions
            do! frameStep (userActions @ gameActions)
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

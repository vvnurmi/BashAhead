module BA

open System
open Actors
open State
open Update

let state = stateUnit
let random = System.Random()
let chooseOne (list : List<string>) = list.[random.Next(list.Length)]

let monsterNames = ["orc"; "goblin"; "wolf"]
let createMonster =
    stateM {
        let! id = getNewId
        return {
            id = id;
            name = chooseOne monsterNames;
            maxhitpoints = 12;
            hitpoints = 12;
        }
    }
let createHero =
    stateM {
        let! id = getNewId
        return {
            id = id;
            name = "Hero";
            maxhitpoints = 42;
            hitpoints = 42;
        }
    }

let showCreature c =
    printfn "%s (%i/%i)" c.name c.hitpoints c.maxhitpoints

let rec uiLoop () =
    stateM {
        do! lift showCreature getHero
        let! monsters = getMonsters
        do for m in monsters do showCreature m
        do! updateState
        let! gameOver = getGameOver
        if gameOver <> null then
            printfn "Game over. %s" gameOver
            Console.ReadLine () |> ignore
            return ()
        else
            match Console.ReadLine() with
            | "q" -> return ()
            | _ -> return! uiLoop ()
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

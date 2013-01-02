module BA

open IO
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
    let nameElem = c.name, Color.Gray
    let hpElem =
        match c.hitpoints with
        | x when x > c.maxhitpoints -> "Brilliant", Color.White
        | x when x = c.maxhitpoints -> "Ok", Color.Green
        | x when x > c.maxhitpoints / 2 -> "Wounded", Color.Yellow
        | x when x > 0<hp> -> "Critical", Color.Red
        | _ -> "Dead", Color.DarkGray
    let propertyElem = sprintf "[%s]" c.weaponName, Color.Gray
    print [nameElem; hpElem; propertyElem]
let showState =
    stateM {
        do! lift showCreature getHero
        let! monsters = getMonsters
        for m in monsters do showCreature m
    }
let getUserActions () =
    stateM {
        printfc "Thrust, Swing, Quit? " Color.White
        let command = getCommand ()
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
            printfc (sprintf "Game over. %s" gameOver) Color.White
            promptUser ()
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

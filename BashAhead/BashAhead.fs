module BA

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
let getAvailableCommands =
    stateM {
        let! monsters = getMonsters
        let! fleeOk = canHeroFlee
        return [
            if not monsters.IsEmpty then
                if List.forall (fun m -> m.distance > 1) monsters then yield "Advance"
                yield "Back up"
                yield "Thrust"
                yield "Swing"
                if fleeOk then yield "Flee"
            yield "Wait"
            yield "Quit"
        ]
    }
let rec getUserActions () =
    let normalizeCommand (s : string) = s.PadRight(1).Substring(0, 1).ToLowerInvariant()
    stateM {
        let! commands = getAvailableCommands
        let command = System.String.Join(", ", commands) + "?" |> getCommand |> normalizeCommand
        let! hero = getHero
        if List.exists (normalizeCommand >> (=) command) commands then
            match command with
            | "a" -> return [ GainDistance(hero.id, -1) ]
            | "b" -> return [ GainDistance(hero.id, 1) ]
            | "f" -> return [ Flee(hero.id) ]
            | "t" -> return! attackWeakest
            | "s" -> return! attackAll
            | "w" -> return []
            | "q" -> return [ Quit ]
            | _ -> failwithf "Command %s missing" command; return []
        else return! getUserActions ()
    }
let checkGameOver =
    stateM {
        let! gameOver = getGameOver
        if gameOver then getCommand "Game over." |> ignore
        return not gameOver
    }
let rec uiLoop () =
    stateM {
        clear ()
        do! showState
        let! ok = checkGameOver
        if ok then
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

module BashAhead.Commands

open ConsoleIO
open Types
open State
open Conditions

type Command =
    | Advance
    | BackUp
    | Thrust
    | Swing
    | Leap
    | Bounce
    | Capture
    | Flee
    | Wait
    | NextGroup
    | Quit

let monsterCommands = 
    [
        Advance
        BackUp
        Thrust
        Swing
        Leap
        Flee
        Bounce
        Capture
    ]
let idleCommands =
    [
        NextGroup
    ]
let systemCommands =
    [
        Wait
        Quit
    ]
let getCommands =
    rState {
        let! monsters = getMonsters
        return [
            if monsters.IsEmpty then yield! idleCommands else yield! monsterCommands
            yield! systemCommands
        ]
    }
let getName = function
    | Advance -> "Advance"
    | BackUp -> "Back up"
    | Thrust -> "Thrust"
    | Swing -> "Swing"
    | Leap -> "Leap"
    | Bounce -> "Bounce off"
    | Capture -> "Capture"
    | Flee -> "Flee"
    | Wait -> "Wait"
    | NextGroup -> "Find more enemies"
    | Quit -> "Quit"
let explainPrecondition command =
    rState {
        match command with
        | Advance -> return "No monster must stand in the way"
        | Thrust -> return "A monster must be at distance 2-3"
        | Swing -> return "Some monsters must be at distance 2-3"
        | Leap ->
            let! minDistance = leapDistanceMin
            return sprintf "The closest monster must be at distance %i-%i" minDistance leapDistanceMax
        | Bounce -> return sprintf "A monster must be at distance %i or closer" bounceDistanceMax
        | Capture -> return sprintf "A discouraged monster must be at distance %i or closer" captureDistanceMax
        | Flee -> return sprintf "No monster should be closer than %i" fleeDistanceMin
        | _ -> return ""
    }
let testPrecondition c =
    rState {
        let! monsters = getMonsters
        let! hero = getHero
        let! aiState = getAIState
        match c with
        | Advance -> return not monsters.IsEmpty && List.forall (fun m -> m.distance > 1) monsters
        | Flee -> return not monsters.IsEmpty && canFlee monsters
        | Leap -> if monsters.IsEmpty then return false else return! canLeap monsters
        | Bounce -> return not monsters.IsEmpty && canBounce monsters
        | Capture -> return not monsters.IsEmpty && canCapture monsters aiState
        | BackUp -> return not monsters.IsEmpty
        | Thrust | Swing -> return not monsters.IsEmpty && List.exists (isInRange hero.weaponName) monsters
        | _ -> return true
    }
let formatCommand c active =
    rState {
        if active then
            return Row [ StrColor(getName c, Color.White); Str "" ]
        else
            let! explanation = explainPrecondition c
            return Row [ StrColor(getName c, Color.DarkGray); StrColor(explanation, Color.DarkGray) ]
    }
let attackWeakest =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let monstersInRange = List.filter (isInRange hero.weaponName) monsters
        return
            match List.sortBy (fun m -> m.hitpoints) monstersInRange with
            | weakest :: _ -> [ Attack(hero.id, [ weakest.id ], weapon.power) ]
            | _ -> []
    }
let attackSweep =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let targets = monsters |> List.filter (isInRange hero.weaponName) |> Seq.truncate 4 |> Seq.toList
        return [ Attack(hero.id, List.map (fun m -> m.id) targets, weapon.power * 2 / 3) ]
    }
let attackLeap =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let target = List.minBy (fun m -> m.distance) monsters
        let distance = min leapDistanceMax <| target.distance - weapon.rangeMin
        return [
            GainDistance(hero.id, -distance)
            Attack(hero.id, [ target.id ], weapon.power)
        ]
    }
let attackBounce =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let target = List.minBy (fun m -> m.distance) monsters
        return [
            yield Attack(hero.id, [ target.id ], weapon.power / 2)
            if target.distance <= bounceDistanceMax then yield GainDistance(hero.id, 2)
        ]
    }
let attackCapture =
    rState {
        let! target = lift <| List.minBy (fun m -> m.distance) <| getMonsters
        let! aiState = getAIState
        match aiState with
        | AllSurrender | AllFlee -> return [ Action.Capture target.id ]
        | _ -> return []
    }
let execute command =
    rState {
        let! hero = getHero
        match command with
        | Advance -> return [ GainDistance(hero.id, -1) ]
        | BackUp -> return [ GainDistance(hero.id, 1) ]
        | Flee -> return [ Action.Flee(hero.id) ]
        | Leap -> return! attackLeap
        | Bounce -> return! attackBounce
        | Capture -> return! attackCapture
        | Thrust -> return! attackWeakest
        | Swing -> return! attackSweep
        | Wait -> return []
        | NextGroup -> return [ Action.NextGroup ]
        | Quit -> return [ Action.Quit ]
    }

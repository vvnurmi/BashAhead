module Commands

open IO
open Types
open State
open Conditions

type Command =
    | Advance
    | BackUp
    | Thrust
    | Swing
    | Flee
    | Wait
    | Quit

let getCommands =
    [
        yield Advance
        yield BackUp
        yield Thrust
        yield Swing
        yield Flee
        yield Wait
        yield Quit
    ]
let getName = function
    | Advance -> "Advance"
    | BackUp -> "Back up"
    | Thrust -> "Thrust"
    | Swing -> "Swing"
    | Flee -> "Flee"
    | Wait -> "Wait"
    | Quit -> "Quit"
let explainPrecondition = function
    | Advance -> "No monster must stand in the way"
    | BackUp -> ""
    | Thrust -> "A monster must be at distance 2-3"
    | Swing -> "Some monsters must be at distance 2-3"
    | Flee -> "No monster should be closer than 8"
    | Wait -> ""
    | Quit -> ""
let testPrecondition c =
    stateM {
        let! monsters = getMonsters
        let! hero = getHero
        match c with
        | Advance -> return not monsters.IsEmpty && List.forall (fun m -> m.distance > 1) monsters
        | Flee -> return not monsters.IsEmpty && canFlee monsters
        | BackUp -> return not monsters.IsEmpty
        | Thrust | Swing -> return not monsters.IsEmpty && List.exists (isInRange hero.weaponName) monsters
        | _ -> return true
    }
let formatCommand pred c =
    if pred c then
        Row [ StrColor(getName c, Color.White); Str "" ]
    else
        Row [ StrColor(getName c, Color.DarkGray); StrColor(explainPrecondition c, Color.DarkGray) ]
let attackWeakest =
    stateM {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let monstersInRange = List.filter (isInRange hero.weaponName) monsters
        return
            match List.sortBy (fun m -> m.hitpoints) monstersInRange with
            | weakest :: _ -> [ Attack(hero.id, [ weakest.id ], weapon.power, Honorable) ]
            | _ -> []
    }
let attackAll =
    stateM {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let monstersInRange = List.filter (isInRange hero.weaponName) monsters
        return [ Attack(hero.id, List.map (fun m -> m.id) monstersInRange, weapon.power * 2 / 3, Inglorious) ]
    }
let execute command =
    stateM {
        let! hero = getHero
        match command with
        | Advance -> return [ GainDistance(hero.id, -1) ]
        | BackUp -> return [ GainDistance(hero.id, 1) ]
        | Flee -> return [ Action.Flee(hero.id) ]
        | Thrust -> return! attackWeakest
        | Swing -> return! attackAll
        | Wait -> return []
        | Quit -> return [ Action.Quit ]
    }

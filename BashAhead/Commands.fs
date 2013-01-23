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
    | Leap
    | Flee
    | Wait
    | Quit

let getCommands =
    [
        yield Advance
        yield BackUp
        yield Thrust
        yield Swing
        yield Leap
        yield Flee
        yield Wait
        yield Quit
    ]
let getName = function
    | Advance -> "Advance"
    | BackUp -> "Back up"
    | Thrust -> "Thrust"
    | Swing -> "Swing"
    | Leap -> "Leap"
    | Flee -> "Flee"
    | Wait -> "Wait"
    | Quit -> "Quit"
let explainPrecondition = function
    | Advance -> "No monster must stand in the way"
    | BackUp -> ""
    | Thrust -> "A monster must be at distance 2-3"
    | Swing -> "Some monsters must be at distance 2-3"
    | Leap -> sprintf "The closest monster must be at distance %i-%i" leapDistanceMin leapDistanceMax
    | Flee -> sprintf "No monster should be closer than %i" fleeDistanceMin
    | Wait -> ""
    | Quit -> ""
let testPrecondition c =
    rState {
        let! monsters = getMonsters
        let! hero = getHero
        match c with
        | Advance -> return not monsters.IsEmpty && List.forall (fun m -> m.distance > 1) monsters
        | Flee -> return not monsters.IsEmpty && canFlee monsters
        | Leap -> return not monsters.IsEmpty && canLeap monsters
        | BackUp -> return not monsters.IsEmpty
        | Thrust | Swing -> return not monsters.IsEmpty && List.exists (isInRange hero.weaponName) monsters
        | _ -> return true
    }
let formatCommand c active =
    if active then
        Row [ StrColor(getName c, Color.White); Str "" ]
    else
        Row [ StrColor(getName c, Color.DarkGray); StrColor(explainPrecondition c, Color.DarkGray) ]
let attackWeakest =
    rState {
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
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let monstersInRange = List.filter (isInRange hero.weaponName) monsters
        return [ Attack(hero.id, List.map (fun m -> m.id) monstersInRange, weapon.power * 2 / 3, Inglorious) ]
    }
let attackLeap =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let target = List.minBy (fun m -> m.distance) monsters
        return [
            GainDistance(hero.id, weapon.rangeMin - target.distance)
            Attack(hero.id, [ target.id ], weapon.power, Honorable)
        ]
    }
let execute command =
    rState {
        let! hero = getHero
        match command with
        | Advance -> return [ GainDistance(hero.id, -1) ]
        | BackUp -> return [ GainDistance(hero.id, 1) ]
        | Flee -> return [ Action.Flee(hero.id) ]
        | Leap -> return! attackLeap
        | Thrust -> return! attackWeakest
        | Swing -> return! attackAll
        | Wait -> return []
        | Quit -> return [ Action.Quit ]
    }

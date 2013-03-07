﻿module BashAhead.Battle.Commands

open BashAhead.Common
open BashAhead.Common.Commands
open BashAhead.Common.ConsoleIO
open BashAhead.Common.Types
open BashAhead.Common.State
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
    | NextGroup
    | Common of BashAhead.Common.Commands.Command

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
let getCommands =
    rState {
        let! monsters = getMonsters
        return [
            if monsters.IsEmpty then yield! idleCommands else yield! monsterCommands
            yield! List.map (fun c -> Common c) commonCommands
        ]
    }
let getNameRaw = function
    | Advance -> "Advance"
    | BackUp -> "Back up"
    | Thrust -> "Thrust"
    | Swing -> "Swing"
    | Leap -> "Leap"
    | Bounce -> "Bounce off"
    | Capture -> "Capture"
    | Flee -> "Flee"
    | NextGroup -> "Find more enemies"
    | Common c -> BashAhead.Common.Commands.getName c
let getName command =
    rState {
        return getNameRaw command
    }
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
        let! hero = liftCommon getHero
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
            return Row [ StrColor(getNameRaw c, Color.White); Str "" ]
        else
            let! explanation = explainPrecondition c
            return Row [ StrColor(getNameRaw c, Color.DarkGray); StrColor(explanation, Color.DarkGray) ]
    }

let gainDistance monsters distance =
    List.map (fun m -> GainDistance(m.id, distance)) monsters

let attackWeakest =
    rState {
        let! hero = liftCommon getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let monstersInRange = List.filter (isInRange hero.weaponName) monsters
        return
            match List.sortBy (fun m -> m.hitpoints) monstersInRange with
            | weakest :: _ -> [ Attack(Hero, [ Monster weakest.id ], weapon.power) ]
            | _ -> []
    }
let attackSweep =
    rState {
        let! hero = liftCommon getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let targets = monsters |> List.filter (isInRange hero.weaponName) |> Seq.truncate 4 |> Seq.toList
        return [ Attack(Hero, List.map (fun m -> Monster m.id) targets, weapon.power * 2 / 3) ]
    }
let attackLeap =
    rState {
        let! hero = liftCommon getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let target = List.minBy (fun m -> m.distance) monsters
        let distance = min leapDistanceMax <| target.distance - weapon.rangeMin
        let distanceGains = gainDistance monsters -distance
        return distanceGains @ [ Attack(Hero, [ Monster target.id ], weapon.power) ]
    }
let attackBounce =
    rState {
        let! hero = liftCommon getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        let target = List.minBy (fun m -> m.distance) monsters
        return [
            yield Attack(Hero, [ Monster target.id ], weapon.power / 2)
            if target.distance <= bounceDistanceMax then yield! gainDistance monsters 2
        ]
    }
let attackCapture =
    rState {
        let! target = lift <| List.minBy (fun m -> m.distance) <| getMonsters
        let! aiState = getAIState
        match aiState with
        | AllSurrender | AllFlee -> return [ Captured target.id ]
        | _ -> return []
    }
let execute command =
    rState {
        let! hero = liftCommon getHero
        let! monsters = getMonsters
        match command with
        | Advance -> return gainDistance monsters -1
        | BackUp -> return gainDistance monsters 1
        | Flee -> return [ Fled Hero ]
        | Leap -> return! attackLeap
        | Bounce -> return! attackBounce
        | Capture -> return! attackCapture
        | Thrust -> return! attackWeakest
        | Swing -> return! attackSweep
        | NextGroup ->
            let! count = getMonsterCount
            return IncMonsterCount :: List.replicate count CreateMonster
        | Common c ->
            let commonEvents = BashAhead.Common.Commands.execute c
            return List.map (fun e -> Event.Common e) commonEvents
    }

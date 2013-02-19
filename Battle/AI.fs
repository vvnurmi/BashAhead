module BashAhead.Battle.AI

open BashAhead.Common
open BashAhead.Common.Types
open BashAhead.Common.Conditions
open Types
open Conditions
open State

let getMonsterActions m =
    rState {
        let! hero = getHero
        let hWeapon = Map.find hero.weaponName Library.weapons
        let mWeapon = Map.find m.weaponName Library.weapons
        let doInRange x min max =
            match m.distance with
            | d when d < min -> [ GainDistance(m.id, 1) ]
            | d when d > max -> [ GainDistance(m.id, -1) ]
            | d when d <= hWeapon.rangeMax && hWeapon.rangeMax < max -> [ GainDistance(m.id, 1) ]
            | d when min < hWeapon.rangeMin && hWeapon.rangeMin <= d -> [ GainDistance(m.id, -1) ]
            | _ -> x
        let! tactic = getAIState
        let attack = doInRange [ Attack(m.id, [ hero.id ], mWeapon.power) ] mWeapon.rangeMin mWeapon.rangeMax
        match tactic with
        | AllIdle -> return []
        | AllFlee -> return doInRange [ Flee m.id ] fleeDistanceMin System.Int32.MaxValue
        | AllSurrender -> return []
        | AllAttack -> return attack
        | OneAttack attackerId -> return if m.id = attackerId then attack else []
    }
let getGameActions =
    rState {
        let! monsters = getMonsters
        return! adapt2 List.collect getMonsterActions monsters
    }
let getAIChanges =
    rState {
        let! monsters = getMonsters
        if monsters.IsEmpty then return [ ChangeTactic AllIdle ]
        else
            let criticals = List.filter (fun m -> health m.maxHitpoints m.hitpoints <= Critical) monsters
            let panic = criticals.Length > monsters.Length / 2
            let! heroHonor = getHeroHonor
            let tactic =
                match heroHonor, panic with
                | Honorable, false -> OneAttack monsters.[0].id
                | Honorable, true -> AllSurrender
                | Inglorious, false -> AllAttack
                | Inglorious, true -> AllFlee
            return [ ChangeTactic tactic ]
    }

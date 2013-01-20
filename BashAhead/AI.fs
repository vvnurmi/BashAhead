module AI

open Types
open Conditions
open State

let getMonsterActions m =
    rwState {
        let! hero = getHero
        let weapon = Map.find m.weaponName Library.weapons
        let doInRange x min max =
            match m.distance with
            | d when d < min -> [ GainDistance(m.id, 1) ]
            | d when d > max -> [ GainDistance(m.id, -1) ]
            | _ -> x
        let! tactic = getAIState
        match tactic with
        | AllIdle -> return []
        | AllFlee -> return doInRange [ Flee m.id ] fleeDistanceMin System.Int32.MaxValue
        | AllAttack -> return doInRange [ Attack(m.id, [ hero.id ], weapon.power, Honorable) ] weapon.rangeMin weapon.rangeMax
    }
let getGameActions =
    rwState {
        let! monsters = getMonsters
        return! adapt2 List.collect getMonsterActions monsters
    }
let getAIChanges =
    rwState {
        let! monsters = getMonsters
        let criticals = List.filter (fun m -> health m.maxhitpoints m.hitpoints <= Critical) monsters
        let panic = criticals.Length > monsters.Length / 2
        return [ ChangeTactic <| if panic then AllFlee else AllAttack ]
    }

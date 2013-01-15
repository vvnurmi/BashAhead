module AI

open Types
open Conditions
open State

let getMonsterActions m =
    stateM {
        let! hero = getHero
        let weapon = Map.find m.weaponName Library.weapons
        let doInRange x min max =
            match m.distance with
            | d when d < min -> [ GainDistance(m.id, 1) ]
            | d when d > max -> [ GainDistance(m.id, -1) ]
            | _ -> x
        let! tactic = getState <| fun state -> state.aiState
        match tactic with
        | AllFlee -> return doInRange [ Flee m.id ] fleeDistanceMin System.Int32.MaxValue
        | AllAttack -> return doInRange [ Attack(m.id, hero.id, weapon.power) ] weapon.rangeMin weapon.rangeMax
    }
let getGameActions =
    stateM {
        let! monsters = getMonsters
        return! adapt (fun op -> List.collect op monsters) getMonsterActions
    }
let updateAI =
    stateM {
        let! monsters = getMonsters
        let criticals = List.filter (fun m -> health m.maxhitpoints m.hitpoints <= Critical) monsters
        let panic = criticals.Length > monsters.Length / 2
        do! mapState <| fun state -> { state with aiState = if panic then AllFlee else AllAttack }
    }

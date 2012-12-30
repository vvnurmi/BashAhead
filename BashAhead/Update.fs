module Update

open Actors
open State

let getActions =
    stateM {
        let! hero = getHero
        return [ Attack(hero.id, 2) ]
    }
let applyAction action =
    stateM {
        return
            match action with
            | Attack(target, power) -> [ GetHit(target, power) ]
    }
let applyChange change =
    stateM {
        match change with
        | GetHit(victimId, power) ->
            let! victim = getCreature victimId
            do! setCreature victimId { victim with hitpoints = victim.hitpoints - power }
    }
let updateState =
    stateM {
        let! actions = getActions
        let! changes = adapt (fun op -> List.collect op actions) applyAction
        do! adapt (fun op -> List.iter op changes) applyChange
    }

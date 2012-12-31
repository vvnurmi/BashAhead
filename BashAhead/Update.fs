module Update

open Actors
open State

let getGameActions =
    stateM {
        let! hero = getHero
        return [ Attack(hero.id, 2) ]
    }
let applyAction action =
    stateM {
        return
            match action with
            | Attack(target, power) -> [ GetHit(target, power) ]
            | Quit -> failwith "Quit"
    }
let applyChange change =
    stateM {
        match change with
        | GetHit(victimId, power) ->
            let! victim = getCreature victimId
            let newHitpoints = victim.hitpoints - power
            do! setCreature victimId { victim with hitpoints = newHitpoints }
            return if newHitpoints <= 0 then [ Die(victimId) ] else []
        | Die(victimId) ->
            let! cType = identify victimId
            match cType with
            | Hero -> do! setGameOver "You died!"
            | Monster -> do! removeMonster victimId
            return []
    }
let rec applyChanges changes =
    stateM {
        let! changes = adapt (fun op -> List.collect op changes) applyChange
        if not changes.IsEmpty then return! applyChanges changes
    }
let updateState actions =
    stateM {
        let! changes = adapt (fun op -> List.collect op actions) applyAction
        do! applyChanges changes
    }

let attackWeakest =
    stateM {
        let! monsters = getMonsters
        return
            match (List.sortBy (fun m -> m.hitpoints) monsters) with
            | weakest :: _ -> [ Attack(weakest.id, 2) ]
            | _ -> []
    }

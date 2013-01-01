module Update

open Types
open State

let getMonsterActions m =
    stateM {
        let! hero = getHero
        let weapon = Map.find m.weaponName Library.weapons
        return [ Attack(hero.id, weapon.power) ]
    }
let getGameActions =
    stateM {
        let! monsters = getMonsters
        return! adapt (fun op -> List.collect op monsters) getMonsterActions
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
            return if newHitpoints <= 0<hp> then [ Die(victimId) ] else []
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
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        return
            match (List.sortBy (fun m -> m.hitpoints) monsters) with
            | weakest :: _ -> [ Attack(weakest.id, weapon.power) ]
            | _ -> []
    }
let attackAll =
    stateM {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        return List.map (fun m -> Attack(m.id, weapon.power * 2 / 3)) monsters
    }

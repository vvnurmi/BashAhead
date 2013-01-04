module Update

open Types
open State

let getMonsterActions m =
    stateM {
        let! hero = getHero
        let weapon = Map.find m.weaponName Library.weapons
        return [ Attack(m.id, hero.id, weapon.power) ]
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
            | Attack(actor, target, power) -> [ WeaponKnown(actor); GetHit(target, power) ]
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
        | WeaponKnown(actorId) ->
            do! updateCreature (fun actor -> { actor with weaponKnown = true }) actorId
            return []
        | Die(victimId) ->
            let! cType = identify victimId
            match cType with
            | Hero ->
                do! addMessage "You draw your terminal breath!"
                do! setGameOver
            | Monster ->
                let! c = getCreature victimId
                do! addMessage <| sprintf "Life escapes %s!" c.name
                do! removeMonster victimId
            return []
    }
let preprocessChanges =
    let deaths = ref Set.empty
    let changeFilter = function
        | Die(_) as c ->
            let skip = (!deaths).Contains c
            deaths := (!deaths).Add c
            not skip
        | _ -> true
    List.filter changeFilter
let rec applyChanges changes =
    stateM {
        let! changes = adapt (fun op -> List.collect op changes) applyChange
        if not changes.IsEmpty then do! changes |> preprocessChanges |> applyChanges 
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
            match List.sortBy (fun m -> m.hitpoints) monsters with
            | weakest :: _ -> [ Attack(hero.id, weakest.id, weapon.power) ]
            | _ -> []
    }
let attackAll =
    stateM {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        let! monsters = getMonsters
        return List.map (fun m -> Attack(hero.id, m.id, weapon.power * 2 / 3)) monsters
    }

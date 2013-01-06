module Update

open Types
open State

let fleeDistanceMin = 8
let canFlee distance =
    distance >= fleeDistanceMin
let canHeroFlee =
    stateM {
        let! monsters = getMonsters
        let distance = monsters |> List.map (fun m -> m.distance) |> List.min
        return canFlee distance
    }

let getMonsterActions m =
    stateM {
        let! hero = getHero
        let weapon = Map.find m.weaponName Library.weapons
        let doInRange x min max =
            match m.distance with
            | d when d < min -> [ GainDistance(m.id, 1) ]
            | d when d > max -> [ GainDistance(m.id, -1) ]
            | _ -> x
        if m.hitpoints < m.maxhitpoints && m.hitpoints <= 8<hp> then
            return doInRange [ Flee m.id ] fleeDistanceMin System.Int32.MaxValue
        else
            return doInRange [ Attack(m.id, hero.id, weapon.power) ] weapon.rangeMin weapon.rangeMax
    }
let getGameActions =
    stateM {
        let! monsters = getMonsters
        return! adapt (fun op -> List.collect op monsters) getMonsterActions
    }
let applyAction action =
    stateM {
        match action with
        | Attack(actorId, targetId, power) ->
            let! a = getCreature actorId
            let! t = getCreature targetId
            let distance = max a.distance t.distance
            let w = Map.find a.weaponName Library.weapons
            if w.rangeMin <= distance && distance <= w.rangeMax then
                return [ WeaponKnown actorId; GetHit(targetId, power) ]
            else
                do! addMessage <| sprintf "%s misses." a.name
                return []
        | GainDistance(actorId, delta) ->
            let! c = getCreature actorId
            let! cType = identify actorId
            match cType with
            | Hero ->
                let! monsters = getMonsters
                return List.map (fun m -> Move(m.id, delta)) monsters
            | Monster ->
                return [ Move(actorId, delta) ]
        | Flee actorId ->
            let! cType = identify actorId
            let tryFlee ok failStr =
                stateM {
                    if ok then
                        return [ Escape actorId ]
                    else
                        do! addMessage failStr
                        return []
                }
            match cType with
            | Hero ->
                let! ok = canHeroFlee
                return! tryFlee ok "You try to flee but monsters are too near."
            | Monster ->
                let! m = getCreature actorId
                let failStr = sprintf "%s fails to escape your attention." m.name
                return! tryFlee (canFlee m.distance) failStr
        | Quit ->
            failwith "Quit"
            return []
    }
let getHitInfo = function
    | x when x >= 3<hp> -> "heavily"
    | x when x >= 2<hp> -> "moderately"
    | x when x >= 1<hp> -> "lightly"
    | _ -> "negligibly"
let applyChange change =
    stateM {
        match change with
        | GetHit(victimId, power) ->
            let! victim = getCreature victimId
            let newHitpoints = victim.hitpoints - power
            do! setCreature victimId { victim with hitpoints = newHitpoints }
            do! addMessage <| sprintf "%s was hit %s." victim.name (getHitInfo power)
            return if newHitpoints <= 0<hp> then [ Die victimId ] else []
        | WeaponKnown actorId ->
            do! updateCreature (fun actor -> { actor with weaponKnown = true }) actorId
            return []
        | Move(actorId, d) ->
            do! updateCreature (fun actor ->
                { actor with distance = max 0 actor.distance + d }) actorId
            return []
        | Escape actorId ->
            let! cType = identify actorId
            match cType with
            | Hero ->
                do! addMessage "You escape the battle!"
                do! setGameOver
                return []
            | Monster ->
                let! c = getCreature actorId
                do! addMessage <| sprintf "%s flees!" c.name
                do! removeMonster actorId
                return []
        | Die victimId ->
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

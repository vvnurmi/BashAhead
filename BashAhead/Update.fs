module BashAhead.Update

open Misc
open Types
open State
open Conditions

let createMonster =
    rwState {
        let! id = getNewId
        return {
            id = id
            name = chooseOne [ "orc"; "goblin"; "wolf" ]
            maxHitpoints = 12<hp>
            hitpoints = 12<hp>
            weaponName = chooseOne <| List.map fst (Map.toList Library.weapons)
            weaponKnown = false
            distance = 10
        }
    }
let handleAction action =
    rState {
        match action with
        | Attack(actorId, [], power) ->
            return []
        | Attack(actorId, targetIds, power) ->
            let! actor = getCreature actorId
            let! actorType = identify actorId
            let! targets = adapt2 List.map getCreature targetIds
            let w = Map.find actor.weaponName Library.weapons
            let applyAttack t =
                let distance = max actor.distance t.distance
                if w.rangeMin <= distance && distance <= w.rangeMax then
                    [ WeaponKnown actorId; GetHit(t.id, power) ]
                else
                    [ Miss(actorId, t.id) ]
            let changes = List.collect applyAttack targets
            let! aiState = getAIState
            let honor =
                match aiState, targets.Length with
                | AllSurrender, _ -> (Inglorious, 4)
                | _, n when n > 1 -> (Inglorious, 1)
                | _, _ -> (Honorable, 1)
            match actorType with
            | Hero -> return HeroHonor honor :: changes
            | Monster -> return changes
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
            let tryFlee ok =
                if ok then [ Escape actorId ] else [ EscapeFail actorId ]
            match cType with
            | Hero ->
                let! monsters = getMonsters
                return tryFlee <| canFlee monsters
            | Monster ->
                let! m = getCreature actorId
                return tryFlee <| canFlee [ m ]
        | NextGroup ->
            let! count = getMonsterCount
            return IncMonsterCount :: List.replicate count CreateMonster
        | Quit ->
            failwith "Quit"
            return []
    }

let mapStateWithMessage get set describe value =
    rwState {
        let! oldValue = get
        do! set value
        let newValue = get
        if oldValue <> value then
            let! description = describe value
            do! addMessage <| sprintf "%s" description
    }
let getHitInfo = function
    | x when x >= 3<hp> -> "heavily"
    | x when x >= 2<hp> -> "moderately"
    | x when x >= 1<hp> -> "lightly"
    | _ -> "negligibly"
let applyChange change =
    rwState {
        match change with
        | GetHit(victimId, power) ->
            let! victim = getCreature victimId
            let newHitpoints = victim.hitpoints - power
            do! setCreature victimId { victim with hitpoints = newHitpoints }
            do! addMessage <| sprintf "%s was hit %s." victim.name (getHitInfo power)
            return if newHitpoints <= 0<hp> then [ Die victimId ] else []
        | Miss(actorId, targetId) ->
            let! actor = getCreature actorId
            let! target = getCreature targetId
            do! addMessage <| sprintf "%s misses %s." actor.name target.name
            return []
        | WeaponKnown actorId ->
            do! updateCreature (fun actor -> { actor with weaponKnown = true }) actorId
            return []
        | Move(actorId, d) ->
            do! updateCreature (fun actor ->
                { actor with distance = max 0 (actor.distance + d) }) actorId
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
        | EscapeFail actorId ->
            let! cType = identify actorId
            let! m = getCreature actorId
            match cType with
            | Hero -> do! addMessage "You try to flee but monsters are too near."
            | Monster -> do! addMessage <| sprintf "%s fails to escape your attention." m.name
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
        | ChangeTactic tactic ->
            do! setAIState tactic
            return []
        | HeroHonor(honor, amplitude) ->
            let describe honor =
                rState {
                    match honor with
                    | Honorable -> return "You are acting honorably!"
                    | Inglorious -> return "Your actions are disgraceful!"
                }
            let! oldHonor = getHeroHonor
            let! oldHonorShift = getHeroHonorShift
            let newHonor, newHonorShift =
                match oldHonorShift with
                | x, n when x = honor && n + amplitude >= 5 -> honor, (honor, 5)
                | x, n when x = honor -> oldHonor, (honor, n + amplitude)
                | x, n when n - amplitude < 1 -> oldHonor, (honor, 1 + amplitude - n)
                | x, n -> oldHonor, (x, n - amplitude)
            do! mapStateWithMessage getHeroHonor setHeroHonor describe newHonor
            do! setHeroHonorShift newHonorShift
            return []
        | IncMonsterCount ->
            let! count = getMonsterCount
            do! setMonsterCount <| count + 1
            return []
        | CreateMonster ->
            let! monster = createMonster
            do! addMonster monster
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
    rwState {
        let! changes = adapt2 List.collect applyChange changes
        if not changes.IsEmpty then do! changes |> preprocessChanges |> applyChanges 
    }
let applyActions actions =
    rwState {
        do! adapt2 List.iter (handleAction %>> applyChanges) actions
    }

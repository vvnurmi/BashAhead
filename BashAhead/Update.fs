module Update

open Types
open State
open Conditions

let handleAction action =
    rState {
        match action with
        | Attack(actorId, targetIds, power, honor) ->
            let! a = getCreature actorId
            let! aType = identify actorId
            let! targets = adapt2 List.map getCreature targetIds
            let w = Map.find a.weaponName Library.weapons
            let applyAttack t =
                let distance = max a.distance t.distance
                if w.rangeMin <= distance && distance <= w.rangeMax then
                    [ WeaponKnown actorId; GetHit(t.id, power) ]
                else
                    [ Miss(actorId, t.id) ]
            let changes = List.collect applyAttack targets
            match aType with
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
            let describe state =
                rState {
                    match state with
                    | AllIdle -> return "The monsters stand idly."
                    | AllAttack -> return "The monsters charge to attack!"
                    | OneAttack mId ->
                        let! m = getCreature mId
                        return sprintf "%s challenges you!" m.name
                    | AllFlee -> return "The monsters flee in panic!"
                }
            do! mapStateWithMessage getAIState setAIState describe tactic
            return []
        | HeroHonor honor ->
            let describe honor =
                rState {
                    match honor with
                    | Honorable -> return "You are acting honorably!"
                    | Inglorious -> return "Your actions are disgraceful!"
                }
            do! mapStateWithMessage getHeroHonor setHeroHonor describe honor
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

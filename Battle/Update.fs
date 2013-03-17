module BashAhead.Battle.Update

open BashAhead.Common
open BashAhead.Common.Misc
open Types
open State
open Conditions

let addMessage s = liftCommon <| BashAhead.Common.State.addMessage s

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
let applyEvent event =
    let attack attacker victim weapon power distance =
        if weapon.rangeMin <= distance && distance <= weapon.rangeMax
        then GetHit(victim, power)
        else Miss(attacker, victim)
    let getHeroHonor = liftCommon getHeroHonor
    let setHeroHonor h = liftCommon <| setHeroHonor h
    rwState {
        match event with
        | Attack(_, [], _) ->
            return []
        | Attack(Hero, targetIds, power) ->
            let! hero = liftCommon getHero
            let w = Map.find hero.weaponName Library.weapons
            let applyAttack t = attack Hero (Monster t.id) w power t.distance
            let! targets = adapt2 List.map getActor targetIds
            let events = List.map applyAttack targets
            let! aiState = getAIState
            let honor =
                match aiState, targetIds.Length with
                | AllSurrender, _ -> (Inglorious, 4)
                | _, n when n > 1 -> (Inglorious, 1)
                | _, _ -> (Honorable, 1)
            return HeroHonor honor :: events
        | Attack(Monster id, [ Hero ], power) ->
            let! actor = getMonster id
            let w = Map.find actor.weaponName Library.weapons
            return WeaponKnown(Monster id) :: [ attack (Monster id) Hero w power actor.distance ]
        | Attack(_, _, _) ->
            failwith "Not implemented"
            return []
        | GainDistance(actorId, delta) ->
            return [ Move(actorId, delta) ]
        | Captured targetId ->
            return [ GoAway targetId; HeroHonor(Honorable, 2) ]
        | Fled Hero ->
            let! monsters = getMonsters
            if canFlee monsters then return [ Escape Hero ] else return [ EscapeFail Hero ]
        | Fled(Monster id as m) ->
            let! creature = getMonster id
            if canFlee [ creature ] then return [ Escape m ] else return [ EscapeFail m ]
        | GetHit(victim, power) ->
            let! c = getActor victim
            let newHitpoints = c.hitpoints - power
            do! setActor { c with hitpoints = newHitpoints } victim
            do! addMessage <| sprintf "%s was hit %s." c.name (getHitInfo power)
            return if newHitpoints <= 0<hp> then [ Die victim ] else []
        | Miss(actor, target) ->
            let! actor = getActor actor
            let! target = getActor target
            do! addMessage <| sprintf "%s misses %s." actor.name target.name
            return []
        | WeaponKnown actor ->
            do! updateActor (fun actor -> { actor with weaponKnown = true }) actor
            return []
        | Move(actor, d) ->
            do! updateActor (fun c ->
                { c with distance = max 0 (c.distance + d) }) <| Monster actor
            return []
        | Escape Hero ->
            do! addMessage "You escape the battle!"
            do! setBattleOver
            return []
        | Escape(Monster id) ->
            let! c = getMonster id
            do! addMessage <| sprintf "%s flees!" c.name
            do! removeMonster id
            return []
        | EscapeFail Hero ->
            do! addMessage "You try to flee but monsters are too near."
            return []
        | EscapeFail(Monster id) ->
            let! c = getMonster id
            do! addMessage <| sprintf "%s fails to escape your attention." c.name
            return []
        | Die Hero ->
            do! addMessage "You draw your terminal breath!"
            do! liftCommon setGameOver
            return []
        | Die(Monster id) ->
            let! c = getMonster id
            do! addMessage <| sprintf "Life escapes %s!" c.name
            do! removeMonster id
            return []
        | GoAway id ->
            let! c = getMonster id
            do! addMessage <| sprintf "You capture %s." c.name
            do! removeMonster id
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
        | Common e ->
            do! liftCommon <| BashAhead.Common.Update.applyEvent e
            return []
    }
let checkState =
    rwState {
        let! gameOver = liftCommon getGameOver
        let! monsters = getMonsters
        if gameOver || monsters.IsEmpty then do! setBattleOver
    }

let preprocessEvents =
    let deaths = ref Set.empty
    let eventFilter = function
        | Die _ as c ->
            let skip = (!deaths).Contains c
            deaths := (!deaths).Add c
            not skip
        | _ -> true
    List.filter eventFilter
let rec applyEvents events =
    rwState {
        let! events = adapt2 List.collect applyEvent events
        if not events.IsEmpty then do! events |> preprocessEvents |> applyEvents 
        do! checkState
    }

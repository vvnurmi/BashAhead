module BashAhead.Battle.State

open BashAhead.Common.Types
open BashAhead.Common.State
open Types

type State = {
    common : BashAhead.Common.State.State
    monsters : Map<MonsterId, Creature>
    heroHonor : Honor
    heroHonorShift : Honor * int // tendency * amplitude
    aiState : AIState
}

let stateUnit = {
    common = BashAhead.Common.State.stateUnit
    monsters = Map.empty
    heroHonor = Honorable
    heroHonorShift = Honorable, 1
    aiState = AllIdle
}

let liftCommon f =
    StateOp <| fun state ->
    let a, commonState2 = rwState.RunOp(f, state.common)
    a, { state with common = commonState2 }

let isDead c =
    c.hitpoints <= 0<hp>

let addMonster c =
    rwState {
        let! id = liftCommon getNewId
        let c = { c with id = id }
        do! mapState <| fun state -> { state with monsters = Map.add id c state.monsters }
    }
let getMonster id =
    getState <| fun state -> Map.find id state.monsters
let setMonster id c =
    mapState <| fun state -> { state with monsters = Map.add id c state.monsters }
let removeMonster id =
    mapState <| fun state -> { state with monsters = Map.remove id state.monsters }

let getActor = function
    | Monster c -> getMonster c
    | Hero -> liftCommon getHero
let setActor c = function
    | Monster id -> setMonster id c
    | Hero -> liftCommon <| setHero c
let updateActor f actor =
    rwState {
        let! c = getActor actor
        do! setActor (f c) actor
    }

let getHeroHonor =
    getState <| fun state -> state.heroHonor
let setHeroHonor h =
    mapState <| fun state -> { state with heroHonor = h }
let getHeroHonorShift =
    getState <| fun state -> state.heroHonorShift
let setHeroHonorShift hs =
    mapState <| fun state -> { state with heroHonorShift = hs }
let getMonsters =
    rState {
        let! monsters = getState <| fun state -> state.monsters
        return List.map snd <| Map.toList monsters
    }
let getAIState =
    getState <| fun state -> state.aiState
let setAIState s =
    mapState <| fun state -> { state with aiState = s }

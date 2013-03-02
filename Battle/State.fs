module BashAhead.Battle.State

open BashAhead.Common.Types
open BashAhead.Common.State
open Types

type State = {
    common : BashAhead.Common.State.State
    nextId : MonsterId
    monsters : Map<MonsterId, Creature>
    heroHonor : Honor
    heroHonorShift : Honor * int // tendency * amplitude
    aiState : AIState
    monsterCount : int
}

let stateUnit = {
    common = BashAhead.Common.State.stateUnit
    nextId = 0
    monsters = Map.empty
    heroHonor = Honorable
    heroHonorShift = Honorable, 1
    aiState = AllIdle
    monsterCount = 1
}

let liftCommon f =
    StateOp <| fun state ->
    let a, commonState2 = rwState.RunOp(f, state.common)
    a, { state with common = commonState2 }

let getNewId =
    StateOp <| fun state ->
    state.nextId, { state with nextId = state.nextId + 1 }
let isDead c =
    c.hitpoints <= 0<hp>

let addMonster c =
    rwState {
        let! id = getNewId
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
let getMonsterCount =
    getState <| fun state -> state.monsterCount
let setMonsterCount n =
    mapState <| fun state -> { state with monsterCount = n }

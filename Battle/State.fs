module BashAhead.Battle.State

open BashAhead.Common.Misc
open BashAhead.Common.Types
open BashAhead.Common.State
open Types

type State = {
    nextId : MonsterId
    monsters : Map<MonsterId, Creature>
    hero : Creature option
    heroHonor : Honor
    heroHonorShift : Honor * int // tendency * amplitude
    gameOver : bool
    messages : string list
    aiState : AIState
    monsterCount : int
}

let stateUnit = {
    nextId = 0
    monsters = Map.empty
    hero = None
    heroHonor = Honorable
    heroHonorShift = Honorable, 1
    gameOver = false
    messages = []
    aiState = AllIdle
    monsterCount = 1
}
let getState f =
    StateOp <| fun state ->
    f state, state
let mapState f =
    StateOp <| fun state ->
    (), f state

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

let getHero =
    rState {
        let! hero = getState <| fun state -> state.hero
        match hero with
        | Some h -> return h
        | _ -> return failwith "Hero not defined"
    }
let setHero h =
    rwState {
        do! mapState <| fun state -> { state with hero = Some h }
    }

let getActor = function
    | Monster c -> getMonster c
    | Hero -> getHero
let setActor c = function
    | Monster id -> setMonster id c
    | Hero -> setHero c
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
let getGameOver =
    getState <| fun state -> state.gameOver
let setGameOver =
    mapState <| fun state -> { state with gameOver = true }
let getMessages =
    getState <| fun state -> state.messages
let addMessage m =
    mapState <| fun state -> { state with messages = capitalize m :: state.messages }
let clearMessages =
    mapState <| fun state -> { state with messages = [] }
let getAIState =
    getState <| fun state -> state.aiState
let setAIState s =
    mapState <| fun state -> { state with aiState = s }
let getMonsterCount =
    getState <| fun state -> state.monsterCount
let setMonsterCount n =
    mapState <| fun state -> { state with monsterCount = n }

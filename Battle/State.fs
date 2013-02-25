module BashAhead.Battle.State

open BashAhead.Common.Misc
open BashAhead.Common.Types
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
type StateOp<'a> = StateOp of (State -> 'a * State)

[<AbstractClass>]
type StateBuilderBase() =
    abstract member RunOp : StateOp<'a> * State -> 'a * State
    member x.Lift(f, mg) =
        StateOp <| fun state ->
        let gResult, state2 = x.RunOp(mg, state)
        f gResult, state2
    member x.Adapt2(f, op, a) =
        StateOp <| fun state ->
        let mutableState = ref state
        let opAdapted q =
            let rOp, newState = x.RunOp(op q, !mutableState)
            mutableState := newState
            rOp
        f opAdapted a, !mutableState
    member x.Adapt3(f, op, a, b) =
        StateOp <| fun state ->
        let mutableState = ref state
        let opAdapted q p =
            let rOp, newState = x.RunOp(op q p, !mutableState)
            mutableState := newState
            rOp
        f opAdapted a b, !mutableState
    member x.Compose(f, g) = fun a ->
        StateOp <| fun state ->
        let rf, state2 = x.RunOp(f a, state)
        x.RunOp(g rf, state2)
    member x.Bind(mf, g) =
        StateOp <| fun state ->
        let rf, state2 = x.RunOp(mf, state)
        x.RunOp(g rf, state2)
    member x.Combine(mf, mg) = x.Bind(mf, fun _ -> mg)
    member x.Return(a) = StateOp <| fun state -> a, state
    member x.ReturnFrom(mf) = StateOp <| fun state -> x.RunOp(mf, state)
    member x.Zero() = StateOp <| fun state -> (), state
    member x.For(s, f) = x.Adapt2(Seq.iter, f, s)
    member x.Delay(f) = f ()

/// Mutable state
type RWStateBuilder() =
    inherit StateBuilderBase()
    override x.RunOp(StateOp f, state) = f state
let rwState = RWStateBuilder()
let lift f mg = rwState.Lift(f, mg)
let adapt2 f op a = rwState.Adapt2(f, op, a)
let adapt3 f op a b = rwState.Adapt3(f, op, a, b)
let (~%) a = rwState.Return a
let (%|>) op opf = rwState.Bind(op, opf)
let (%>>) opf1 opf2 = rwState.Compose(opf1, opf2)

/// Read-only state
type RStateBuilder() =
    inherit StateBuilderBase()
    override x.RunOp(StateOp f, state) =
        let rf, state2 = f state
#if DEBUG
        if state <> state2 then failwith "Illegal state change"
#endif
        rf, state
let rState = RStateBuilder()

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

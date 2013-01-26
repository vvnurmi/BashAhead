module State

open Misc
open Types

type StateT = {
    nextId : int
    creatures : Map<CreatureId, Creature>
    hero : CreatureId option
    heroHonor : Honor
    monsters : CreatureId list
    gameOver : bool
    messages : string list
    aiState : AIState
    monsterCount : int
}
type StateOp<'a> = StateOp of (StateT -> 'a * StateT)

[<AbstractClass>]
type StateBuilderBase() =
    abstract member RunOp : StateOp<'a> * StateT -> 'a * StateT
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
    creatures = Map.empty
    hero = None
    heroHonor = Honorable
    monsters = []
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
let identify id =
    rState {
        let! isHero = getState <| fun state -> Some id = state.hero
        return if isHero then Hero else Monster
    }
let isDead c =
    c.hitpoints <= 0<hp>
let getCreature id =
    getState <| fun state -> Map.find id state.creatures
let setCreature id c =
    mapState <| fun state -> { state with creatures = Map.add id c state.creatures }
let updateCreature f id =
    rwState {
        let! c = getCreature id
        do! setCreature id (f c)
    }
let getHero =
    rState {
        let! hero = getState <| fun state -> state.hero
        match hero with
        | Some c -> return! getCreature c
        | _ -> return failwith "Hero not defined"
    }
let setHero h =
    mapState <| fun state ->
        { state with
            hero = Some(h.id);
            creatures = Map.add h.id h state.creatures }
let getHeroHonor =
    getState <| fun state -> state.heroHonor
let setHeroHonor h =
    mapState <| fun state -> { state with heroHonor = h }
let getMonsters =
    rState {
        let! monsterIds = getState <| fun state -> state.monsters
        return! adapt2 List.map getCreature monsterIds
    }
let addMonster m =
    mapState <| fun state ->
        { state with
            monsters = m.id :: state.monsters;
            creatures = Map.add m.id m state.creatures }
let removeMonster id =
    mapState <| fun state ->
        { state with
            monsters = List.filter ((<>) id) state.monsters }
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

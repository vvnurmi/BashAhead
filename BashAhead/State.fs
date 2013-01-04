module State

open Misc
open Types

type StateT = {
    nextId : int
    creatures : Map<CreatureId, Creature>
    hero : CreatureId option
    monsters : CreatureId list
    gameOver : bool
    messages : string list
}
type StateOp<'a> = StateOp of (StateT -> 'a * StateT)

let stateUnit = {
    nextId = 0
    creatures = Map.empty
    hero = None
    monsters = []
    gameOver = false
    messages = []
}

let getNewId =
    StateOp <| fun state ->
    state.nextId, { state with nextId = state.nextId + 1 }

let run m state =
    match m with
    | StateOp f -> f state
let ret a =
    StateOp <| fun state ->
    a, state
let lift f gOp =
    StateOp <| fun state ->
    let gResult, state2 = run gOp state
    f gResult, state2
let adapt f op =
    StateOp <| fun state ->
    let mutableState = ref state
    let op2 x =
        let rOp, newState = run (op x) !mutableState
        mutableState := newState
        rOp
    f op2, !mutableState

type StateBuilder() =
    member x.Bind(mf, g) =
        StateOp <| fun state ->
        let rf, state2 = run mf state
        run (g rf) state2
    member x.Combine(mf, mg) =
        StateOp <| fun state ->
        let rf, state2 = run mf state
        run mg state2
    member x.Return(a) =
        ret a
    member x.ReturnFrom(mf) =
        StateOp <| fun state ->
        run mf state
    member x.Zero() =
        StateOp <| fun state ->
        (), state
    member x.For(s, f) =
        adapt (fun op -> Seq.iter op s) f
    member x.Delay(f) = f ()

let stateM = StateBuilder()


let getState f =
    StateOp <| fun state ->
    f state, state
let mapState f =
    StateOp <| fun state ->
    (), f state

let identify id =
    stateM {
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
    stateM {
        let! c = getCreature id
        do! setCreature id (f c)
    }
let getHero =
    StateOp <| fun state ->
    match state.hero with
    | Some c -> run (getCreature c) state
    | _ -> failwith "Hero not defined"
let setHero h =
    mapState <| fun state ->
        { state with
            hero = Some(h.id);
            creatures = Map.add h.id h state.creatures }
let getMonsters =
    StateOp <| fun state ->
    run <| adapt (fun op -> List.map op state.monsters) getCreature <| state
let addMonster m =
    mapState <| fun state ->
        { state with
            monsters = m.id :: state.monsters;
            creatures = Map.add m.id m state.creatures }
let removeMonster id =
    mapState <| fun state ->
        { state with
            monsters = List.filter (fun mId -> mId <> id) state.monsters }
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

﻿module BashAhead.Common.State

open Misc
open Types

type StateOp<'s, 'a when 's : equality> =
    StateOp of ('s -> 'a * 's)

[<AbstractClass>]
type StateBuilderBase() =
    abstract member RunOp : StateOp<'s, 'a> * 's -> 'a * 's
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
    member x.ReturnFrom(mf) = mf
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

let getState f =
    StateOp <| fun state ->
    f state, state
let mapState f =
    StateOp <| fun state ->
    (), f state

type State = {
    nextId : int
    hero : Creature option
    heroHonor : Honor
    messages : string list
    gameOver : bool
}

let stateUnit = {
    nextId = 1
    hero = None
    heroHonor = Honorable
    messages = []
    gameOver = false
}

let getNewId =
    StateOp <| fun state ->
    state.nextId, { state with nextId = state.nextId + 1 }
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

let getHeroHonor =
    getState <| fun state -> state.heroHonor
let setHeroHonor h =
    mapState <| fun state -> { state with heroHonor = h }

let getMessages =
    getState <| fun state -> state.messages
let addMessage m =
    mapState <| fun state -> { state with messages = capitalize m :: state.messages }
let clearMessages =
    mapState <| fun state -> { state with messages = [] }

let getGameOver =
    getState <| fun state -> state.gameOver
let setGameOver =
    mapState <| fun state -> { state with gameOver = true }

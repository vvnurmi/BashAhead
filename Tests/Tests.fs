module BashAhead.Tests.Tests

open FsCheck
open FsCheck.Commands
open FsCheck.Prop
open BashAhead.Common.Types
open BashAhead.Common.State
open BashAhead.Battle.Types
open BashAhead.Battle.State
open BashAhead.Battle.Conditions
open BashAhead.Battle.Update

type HeroHonorModel = Honor * int

let heroHonorSpec =
    let shiftMatchesAccu accu = function
        | Honorable, amplitude -> accu = amplitude
        | Inglorious, amplitude -> accu = -amplitude
    let postCondition state (honor, accu) =
        let op = getState <| fun state -> (state.common.heroHonor, state.heroHonorShift)
        let (stateHonor, stateHonorShift), _ = rwState.RunOp(op, state)
        stateHonor = honor && shiftMatchesAccu accu stateHonorShift
    let honorable =
        { new ICommand<State, HeroHonorModel>() with
            member x.RunActual state =
                let op = applyEvents [ HeroHonor(Honorable, 1) ]
                let _, state2 = rwState.RunOp(op, state)
                state2
            member x.RunModel((honor, accu)) =
                if accu = -1 then honor, 1
                else if accu < 4 then honor, accu + 1
                else Honorable, 5
            member x.Post(state, model) = postCondition state model
            override x.ToString() = "honorable" }
    let inglorious =
        { new ICommand<State, HeroHonorModel>() with
            member x.RunActual state =
                let op = applyEvents [ HeroHonor(Inglorious, 1) ]
                let _, state2 = rwState.RunOp(op, state)
                state2
            member x.RunModel((honor, accu)) =
                if accu = 1 then honor, -1
                else if accu > -4 then honor, accu - 1
                else Inglorious, -5
            member x.Post(state, model) = postCondition state model
            override x.ToString() = "inglorious" }
    { new ISpecification<State, HeroHonorModel> with
        member x.Initial() = stateUnit, (Honorable, 1)
        member x.GenCommand _ = Gen.elements [ honorable; inglorious ] }
let ``hero honor changes`` = asProperty heroHonorSpec

type Marker = class end
Check.All(Config.Default, typeof<Marker>.DeclaringType)

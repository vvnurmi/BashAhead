module BashAhead.Explore.State

open BashAhead.Common.State
open BashAhead.Common.Types
open Types

type Location = {
    id : LocationId
    name : string
}

type State = {
    common : BashAhead.Common.State.State
    nextId : LocationId
    locations : Map<LocationId, Location>
    heroLocation : LocationId option
}

let stateUnit = {
    common = BashAhead.Common.State.stateUnit
    nextId = 0
    locations = Map.empty
    heroLocation = None
}

let liftCommon f =
    StateOp <| fun state ->
    let a, commonState2 = rwState.RunOp(f, state.common)
    a, { state with common = commonState2 }

let getLocation id =
    getState <| fun state ->
    Map.find id state.locations
let getLocations =
    rState {
        let! locations = getState <| fun state -> state.locations
        return List.map snd <| Map.toList locations
    }
let addLocation (location : Location) =
    rwState {
        let! id = liftCommon getNewId
        let location = { location with id = id }
        do! mapState <| fun state -> { state with locations = Map.add id location state.locations }
    }

let getHeroLocation =
    getState <| fun state ->
    match state.heroLocation with
    | Some id -> Map.tryFind id state.locations
    | None -> None
let setHeroLocation id =
    mapState <| fun state ->
    { state with heroLocation = Some id }

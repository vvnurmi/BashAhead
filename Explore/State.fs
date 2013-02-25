module BashAhead.Explore.State

open Types

type Location = {
    name : string
}

type State = {
    nextId : LocationId
    locations : Map<LocationId, Location>
    heroLocation : LocationId option
}

let stateUnit = {
    nextId = 0
    locations = Map.empty
    heroLocation = None
}

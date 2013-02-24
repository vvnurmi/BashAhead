module BashAhead.Explore.State

type LocationId = int

type Location = {
    name : string
}

type State = {
    locations : Map<LocationId, Location>
    heroLocation : LocationId
}

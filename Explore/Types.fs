module BashAhead.Explore.Types

type LocationId = int

type Event =
    | HeroMoves of LocationId
    | Common of BashAhead.Common.Types.Event

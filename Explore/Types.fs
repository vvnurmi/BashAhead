module BashAhead.Explore.Types

type LocationId = int

type Event =
    | ToBattle
    | HeroMoves of LocationId
    | Common of BashAhead.Common.Types.Event

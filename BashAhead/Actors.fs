module Actors

type CreatureId = int
type Creature = {
    id : CreatureId;
    name : string;
    maxhitpoints : int;
    hitpoints : int
}
type Action =
    | Attack of CreatureId * int
    | Quit
type Change =
    | GetHit of CreatureId * int
    | Die of CreatureId
type CreatureType =
    | Hero
    | Monster

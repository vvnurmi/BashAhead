module Actors

[<Measure>] type hp

type CreatureId = int
type Creature = {
    id : CreatureId;
    name : string;
    maxhitpoints : int<hp>;
    hitpoints : int<hp>;
}
type Action =
    | Attack of CreatureId * int<hp>
    | Quit
type Change =
    | GetHit of CreatureId * int<hp>
    | Die of CreatureId
type CreatureType =
    | Hero
    | Monster

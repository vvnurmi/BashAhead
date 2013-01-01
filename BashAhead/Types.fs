module Types

[<Measure>] type hp

type Weapon = {
    name : string;
}
type CreatureId = int
type Creature = {
    id : CreatureId;
    name : string;
    maxhitpoints : int<hp>;
    hitpoints : int<hp>;
    weaponName : string;
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

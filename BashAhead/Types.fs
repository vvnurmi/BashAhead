module Types

[<Measure>] type hp

type Weapon = {
    name : string
    power : int<hp>
}
type CreatureId = int
type Creature = {
    id : CreatureId
    name : string
    maxhitpoints : int<hp>
    hitpoints : int<hp>
    weaponName : string
    weaponKnown : bool
}
type Action =
    | Attack of CreatureId * CreatureId * int<hp> // actor * victim * power
    | Quit
type Change =
    | GetHit of CreatureId * int<hp>
    | WeaponKnown of CreatureId
    | Die of CreatureId
type CreatureType =
    | Hero
    | Monster

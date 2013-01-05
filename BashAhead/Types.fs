module Types

[<Measure>] type hp

type Weapon = {
    name : string
    power : int<hp>
    rangeMin : int
    rangeMax : int
}
type CreatureId = int
type Creature = {
    id : CreatureId
    name : string
    maxhitpoints : int<hp>
    hitpoints : int<hp>
    weaponName : string
    weaponKnown : bool
    distance : int
}
type Action =
    | Attack of CreatureId * CreatureId * int<hp> // actor * victim * power
    | GainDistance of CreatureId * int
    | Quit
type Change =
    | GetHit of CreatureId * int<hp>
    | WeaponKnown of CreatureId
    | Move of CreatureId * int
    | Die of CreatureId
type CreatureType =
    | Hero
    | Monster

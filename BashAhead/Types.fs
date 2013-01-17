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
type AIState =
    | AllAttack
    | AllFlee
type Action =
    | Attack of CreatureId * CreatureId * int<hp> // actor * victim * power
    | GainDistance of CreatureId * int
    | Flee of CreatureId
    | Quit
type Change =
    | GetHit of CreatureId * int<hp>
    | Miss of CreatureId * CreatureId // actor * target
    | WeaponKnown of CreatureId
    | Move of CreatureId * int
    | Escape of CreatureId
    | EscapeFail of CreatureId
    | Die of CreatureId
    | ChangeTactic of AIState
type CreatureType =
    | Hero
    | Monster

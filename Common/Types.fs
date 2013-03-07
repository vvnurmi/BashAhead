module BashAhead.Common.Types

[<Measure>] type hp

type Weapon = {
    name : string
    power : int<hp>
    rangeMin : int
    rangeMax : int
}
type MonsterId = int
type Creature = {
    id : MonsterId
    name : string
    maxHitpoints : int<hp>
    hitpoints : int<hp>
    weaponName : string
    weaponKnown : bool
    distance : int
}
type Honor =
    | Honorable
    | Inglorious
type Event =
    | Quit

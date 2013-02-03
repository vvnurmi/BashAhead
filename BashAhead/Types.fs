module BashAhead.Types

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
    maxHitpoints : int<hp>
    hitpoints : int<hp>
    weaponName : string
    weaponKnown : bool
    distance : int
}
type AIState =
    | AllIdle
    | OneAttack of CreatureId // attacker
    | AllAttack
    | AllFlee
    | AllSurrender
type Honor =
    | Honorable
    | Inglorious
type Action =
    | Attack of CreatureId * CreatureId list * int<hp> // actor * victims * power
    | GainDistance of CreatureId * int
    | Flee of CreatureId
    | NextGroup
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
    | HeroHonor of Honor * int // type * amplitude
    | IncMonsterCount
    | CreateMonster
type CreatureType =
    | Hero
    | Monster

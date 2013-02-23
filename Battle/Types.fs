module BashAhead.Battle.Types

open BashAhead.Common.Types

type Actor =
    | Hero
    | Monster of CreatureId
type AIState =
    | AllIdle
    | OneAttack of CreatureId // attacker
    | AllAttack
    | AllFlee
    | AllSurrender
type Action =
    | Attack of Actor * Actor list * int<hp> // attacker * victims * power
    | GainDistance of CreatureId * int
    | Capture of CreatureId
    | Flee of Actor
    | NextGroup
    | Quit
type Change =
    | GetHit of Actor * int<hp>
    | Miss of Actor * Actor // attacker * victim
    | WeaponKnown of Actor
    | Move of CreatureId * int
    | Escape of Actor
    | EscapeFail of Actor
    | Die of Actor
    | GoAway of CreatureId
    | ChangeTactic of AIState
    | HeroHonor of Honor * int // type * amplitude
    | IncMonsterCount
    | CreateMonster

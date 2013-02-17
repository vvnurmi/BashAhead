module BashAhead.Types

open BashAhead.Common.Types

type AIState =
    | AllIdle
    | OneAttack of CreatureId // attacker
    | AllAttack
    | AllFlee
    | AllSurrender
type Action =
    | Attack of CreatureId * CreatureId list * int<hp> // actor * victims * power
    | GainDistance of CreatureId * int
    | Capture of CreatureId
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
    | GoAway of CreatureId
    | ChangeTactic of AIState
    | HeroHonor of Honor * int // type * amplitude
    | IncMonsterCount
    | CreateMonster

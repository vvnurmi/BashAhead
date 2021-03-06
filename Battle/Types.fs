﻿module BashAhead.Battle.Types

open BashAhead.Common.Types

type Actor =
    | Hero
    | Monster of MonsterId
type AIState =
    | AllIdle
    | OneAttack of MonsterId // attacker
    | AllAttack
    | AllFlee
    | AllSurrender
type Event =
    | Attack of Actor * Actor list * int<hp> // attacker * victims * power
    | GainDistance of MonsterId * int
    | Captured of MonsterId
    | Fled of Actor
    | GetHit of Actor * int<hp>
    | Miss of Actor * Actor // attacker * victim
    | WeaponKnown of Actor
    | Move of MonsterId * int
    | Escape of Actor
    | EscapeFail of Actor
    | Die of Actor
    | GoAway of MonsterId
    | ChangeTactic of AIState
    | HeroHonor of Honor * int // type * amplitude
    | Common of BashAhead.Common.Types.Event

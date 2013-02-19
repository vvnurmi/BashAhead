module BashAhead.Common.Conditions

open Types

type HealthClass =
    | Dead
    | Critical
    | Wounded
    | Bruised
    | Ok
    | Brilliant
let health maxHitpoints hitpoints =
    if maxHitpoints < 30<hp> then
        match hitpoints with
        | x when x > maxHitpoints -> Brilliant
        | x when x = maxHitpoints -> Ok
        | x when x > maxHitpoints / 2 -> Wounded
        | x when x > 0<hp> -> Critical
        | _ -> Dead
    else
        match hitpoints with
        | x when x > maxHitpoints -> Brilliant
        | x when x = maxHitpoints -> Ok
        | x when x > maxHitpoints * 2 / 3 -> Bruised
        | x when x > maxHitpoints * 1 / 3 -> Wounded
        | x when x > 0<hp> -> Critical
        | _ -> Dead

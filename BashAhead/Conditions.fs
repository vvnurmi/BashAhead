module Conditions

open Types

let minDistance =
    List.map (fun c -> c.distance) >> List.min

let fleeDistanceMin = 8
let canFlee monsters =
    minDistance monsters >= fleeDistanceMin

let leapDistanceMin = 4
let leapDistanceMax = 6
let canLeap monsters =
    let distance = minDistance monsters
    leapDistanceMin <= distance && distance <= leapDistanceMax

let isInRange weaponName monster =
    let weapon = Map.find weaponName Library.weapons
    weapon.rangeMin <= monster.distance && monster.distance <= weapon.rangeMax

type HealthClass =
    | Dead
    | Critical
    | Wounded
    | Ok
    | Brilliant
let health maxHitpoints hitpoints =
    match hitpoints with
    | x when x > maxHitpoints -> Brilliant
    | x when x = maxHitpoints -> Ok
    | x when x > maxHitpoints / 2 -> Wounded
    | x when x > 0<hp> -> Critical
    | _ -> Dead

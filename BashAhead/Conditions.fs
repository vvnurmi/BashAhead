module Conditions

open Types

let fleeDistanceMin = 8
let canFlee monsters =
    let distance = monsters |> List.map (fun c -> c.distance) |> List.min
    distance >= fleeDistanceMin

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

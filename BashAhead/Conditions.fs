module Conditions

open Types

let fleeDistanceMin = 8
let canFlee monsters =
    let distance = monsters |> List.map (fun c -> c.distance) |> List.min
    distance >= fleeDistanceMin

let isInRange weaponName monster =
    let weapon = Map.find weaponName Library.weapons
    weapon.rangeMin <= monster.distance && monster.distance <= weapon.rangeMax

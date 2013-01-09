module Conditions

open Types

let fleeDistanceMin = 8
let canFlee monsters =
    let distance = monsters |> List.map (fun c -> c.distance) |> List.min
    distance >= fleeDistanceMin

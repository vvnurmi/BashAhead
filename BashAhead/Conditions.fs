module BashAhead.Conditions

open Types
open State

let minDistance =
    List.map (fun c -> c.distance) >> List.min

let fleeDistanceMin = 8
let canFlee monsters =
    minDistance monsters >= fleeDistanceMin

let leapDistanceMin =
    rState {
        let! hero = getHero
        let weapon = Map.find hero.weaponName Library.weapons
        return weapon.rangeMax + 1
    }
let leapDistanceMax = 6
let canLeap monsters =
    rState {
        let distance = minDistance monsters
        let! minDistance = leapDistanceMin
        return minDistance <= distance && distance <= leapDistanceMax
    }

let bounceDistanceMax = 2
let canBounce monsters =
    minDistance monsters <= bounceDistanceMax

let isInRange weaponName monster =
    let weapon = Map.find weaponName Library.weapons
    weapon.rangeMin <= monster.distance && monster.distance <= weapon.rangeMax

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

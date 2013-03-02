module BashAhead.Battle.Conditions

open BashAhead.Common
open BashAhead.Common.Types
open Types
open State

let minDistance =
    List.map (fun c -> c.distance) >> List.min

let fleeDistanceMin = 8
let canFlee monsters =
    minDistance monsters >= fleeDistanceMin

let leapDistanceMin =
    rState {
        let! hero = liftCommon getHero
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

let captureDistanceMax = 2
let canCapture monsters aiState =
    (aiState = AllSurrender || aiState = AllFlee) && minDistance monsters <= captureDistanceMax

let isInRange weaponName monster =
    let weapon = Map.find weaponName Library.weapons
    weapon.rangeMin <= monster.distance && monster.distance <= weapon.rangeMax

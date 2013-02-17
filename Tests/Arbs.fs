module BashAhead.Tests.Arbs

open FsCheck
open BashAhead.Common
open BashAhead.Common.Types

let creature hitpoints weaponName distance =
    {
        id = 0
        name = "foo"
        maxHitpoints = hitpoints
        hitpoints = hitpoints
        weaponName = weaponName
        weaponKnown = false
        distance = distance
    }
let hitpointsGen =
    Gen.map <| (*) 1<hp> <| Gen.sized (fun s -> Gen.choose(0, s))
let weaponNameGen =
    Gen.map (List.nth Library.weaponNames) <| Gen.choose(0, Library.weaponNames.Length - 1)
let creatureGen =
    Gen.map3 creature hitpointsGen weaponNameGen (Gen.sized <| fun s -> Gen.choose(0, s))
let creatureShrinker creature =
    seq {
        if creature.hitpoints > 0<hp> then
            yield
                { creature with
                    hitpoints = creature.hitpoints / 2
                    maxHitpoints = creature.maxHitpoints / 2 }
        if creature.distance > 0 then
            yield { creature with distance = creature.distance / 2 }
            yield { creature with distance = creature.distance - 1 }
    }
type Generators =
    static member Creature =
        { new Arbitrary<Creature>() with
            override x.Generator = creatureGen
            override x.Shrinker c = creatureShrinker c }
Arb.register<Generators> () |> ignore

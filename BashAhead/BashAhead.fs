module BashAhead.BA

[<assembly:System.Runtime.InteropServices.ComVisible false>]
[<assembly:System.Reflection.AssemblyVersion "1.0.*">]
[<assembly:System.CLSCompliant true>]
do ()

open BashAhead.Common.Types
open BashAhead.Common.State
open BashAhead.Explore.State

let createHero () =
    {
        id = 0
        name = "Hero"
        maxHitpoints = 42<hp>
        hitpoints = 42<hp>
        weaponName = "sword"
        weaponKnown = true
        distance = 0
    }
let initialize () =
    rwState {
        let hero = createHero ()
        do! liftCommon <| setHero hero
        let locations = List.map (fun name -> { id = 0; name = name }) [ "village"; "cave"; "fields"; "forest"; "mountain"; "lake"; "river" ]
        do! adapt2 List.iter addLocation locations
        let! locations = getLocations
        do! setHeroLocation locations.[0].id
    }
let main =
    rwState {
        do! initialize ()
        do! BashAhead.Explore.ConsoleIO.uiLoop ()
    }
let _, finalState = rwState.RunOp(main, BashAhead.Explore.State.stateUnit)

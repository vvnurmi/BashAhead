module BashAhead.BA

[<assembly:System.Runtime.InteropServices.ComVisible false>]
[<assembly:System.Reflection.AssemblyVersion "1.0.*">]
[<assembly:System.CLSCompliant true>]
do ()

open BashAhead.Common.Types
open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
open BashAhead.Explore.State
open BashAhead.Explore.ConsoleIO

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
// Note: liftCommon and liftCommon2 are supposed to be the same generic function.
// It is needed twice in order to bind it to two different type signatures.
let rec uiLoop showState processUI liftCommon liftCommon2 =
    rwState {
        clear ()
        do! showState
        do! liftCommon clearMessages
        let! proceed = liftCommon2 checkGameOver
        if proceed then
            do! processUI ()
            do! uiLoop showState processUI liftCommon liftCommon2
    }
let main =
    rwState {
        do! initialize ()
        do! uiLoop showState processUI liftCommon liftCommon
    }
let _, finalState = rwState.RunOp(main, BashAhead.Explore.State.stateUnit)

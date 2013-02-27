module BashAhead.BA

[<assembly:System.Runtime.InteropServices.ComVisible false>]
[<assembly:System.Reflection.AssemblyVersion "1.0.*">]
[<assembly:System.CLSCompliant true>]
do ()

open BashAhead.Common.Types
open BashAhead.Common.State
open BashAhead.Battle.State
open BashAhead.Battle.ConsoleIO

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

let rec uiLoop () =
    rwState {
        let! proceed = processUI ()
        if proceed then do! uiLoop ()
    }
let main =
    rwState {
        do! setHero <| createHero ()
        do! uiLoop ()
    }
let _, finalState = rwState.RunOp(main, stateUnit)

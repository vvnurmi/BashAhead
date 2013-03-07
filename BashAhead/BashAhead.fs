﻿module BashAhead.BA

[<assembly:System.Runtime.InteropServices.ComVisible false>]
[<assembly:System.Reflection.AssemblyVersion "1.0.*">]
[<assembly:System.CLSCompliant true>]
do ()

open BashAhead.Common.Types
open BashAhead.Common.State
open BashAhead.Common.ConsoleIO
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

let rec uiLoop showState processUI =
    rwState {
        clear ()
        do! showState
        do! liftCommon clearMessages
        let! proceed = liftCommon checkGameOver
        if proceed then
            do! processUI ()
            do! uiLoop showState processUI
    }
let main =
    rwState {
        let hero = createHero ()
        do! liftCommon <| setHero hero
        do! uiLoop BashAhead.Battle.ConsoleIO.showState BashAhead.Battle.ConsoleIO.processUI
    }
let _, finalState = rwState.RunOp(main, stateUnit)

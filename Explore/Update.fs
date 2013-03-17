module BashAhead.Explore.Update

open BashAhead.Common.Misc
open BashAhead.Common.Types
open BashAhead.Common.State
open Types
open State

let createMonster () =
    {
        id = 0
        name = chooseOne [ "orc"; "goblin"; "wolf" ]
        maxHitpoints = 12<hp>
        hitpoints = 12<hp>
        weaponName = chooseOne <| List.map fst (Map.toList BashAhead.Common.Library.weapons)
        weaponKnown = false
        distance = 10
    }
let doBattle =
    rwState {
        let monsterCount = random.Next(1, 5)
        let monsters = List.init monsterCount <| fun _ -> createMonster ()
        do! adapt2 List.iter BashAhead.Battle.State.addMonster monsters
        do! BashAhead.Battle.ConsoleIO.uiLoop ()
    }
let applyEvent event =
    rwState {
        match event with
        | ToBattle ->
            let! battleState = extractBattleState
            let _, battleState2 = rwState.RunOp(doBattle, battleState)
            do! absorbBattleState battleState2
        | HeroMoves id -> do! setHeroLocation id
        | Common e -> do! liftCommon <| BashAhead.Common.Update.applyEvent e
    }

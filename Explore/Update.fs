module BashAhead.Explore.Update

open BashAhead.Common.State
open Types
open State

let applyEvent event =
    rwState {
        match event with
        | ToBattle -> do ()
        | HeroMoves id -> do! setHeroLocation id
        | Common e -> do! liftCommon <| BashAhead.Common.Update.applyEvent e
    }

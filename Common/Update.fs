module BashAhead.Common.Update

open Types
open State

let applyEvent event =
    rwState {
        match event with
        | Quit -> do! setGameOver
    }

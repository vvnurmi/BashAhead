module Update

open Actors
open State

let getActions =
    StateOp(fun state ->
    match state.hero with
    | Some h -> ([ Attack(h, 2) ], state)
    | _ -> ([], state))
let applyAction action =
    stateM {
        return
            match action with
            | Attack(target, power) -> [ GetHit(target, power) ]
    }
let rec applyChanges changes =
    stateM {
        match changes with
        | GetHit(victimId, power) :: tail ->
            let! victim = getCreature victimId
            do! setCreature victimId { victim with hitpoints = victim.hitpoints - power }
            return! applyChanges tail
        | [] -> return ()
    }
let updateState =
    stateM {
        let! actions = getActions
        let! changes = adapt (fun op -> List.collect op actions) applyAction
        do! applyChanges changes
    }

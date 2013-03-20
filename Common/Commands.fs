module BashAhead.Common.Commands

open Types

type Command =
    | Wait
    | Quit

let commonCommands =
    [
        Wait
        Quit
    ]
let getName = function
    | Wait -> "Wait"
    | Quit -> "Quit"
let execute command parameters =
    match command, parameters with
    | Wait, [] -> []
    | Quit, [] -> [ Event.Quit ]
    | x -> failwith <| sprintf "Invalid command %O" x

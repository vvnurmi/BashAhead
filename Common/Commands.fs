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
let execute = function
    | Wait -> []
    | Quit -> [ Event.Quit ]

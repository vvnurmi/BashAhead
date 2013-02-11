module BashAhead.Tests

open FsCheck
open Conditions

let ``can bounce one step`` monsters = not <| List.isEmpty monsters ==> lazy canBounce monsters

type Marker = class end
Check.All(Config.Default, typeof<Marker>.DeclaringType)

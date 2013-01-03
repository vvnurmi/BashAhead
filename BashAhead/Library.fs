module Library

open Types

let weapons =
    let weaponsList = [
        { name = "bow"; power = 2<hp> };
        { name = "sword"; power = 3<hp> };
        { name = "fangs"; power = 1<hp> };
    ]
    weaponsList |> List.map (fun w -> w.name, w) |> Map.ofList

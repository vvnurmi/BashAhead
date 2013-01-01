module Library

open Types

let weapons =
    let weaponsList = [
        { name = "bow"; power = 2<hp> };
        { name = "sword"; power = 3<hp> };
        { name = "fangs"; power = 1<hp> };
    ]
    Map.ofList (List.map (fun (w : Weapon) -> w.name, w) weaponsList)

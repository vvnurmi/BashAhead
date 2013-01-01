module Library

open Types

let weapons =
    let weaponsList = [
        { Weapon.name = "bow" };
        { Weapon.name = "sword" };
        { Weapon.name = "fangs" };
    ]
    Map.ofList (List.map (fun (w : Weapon) -> w.name, w) weaponsList)

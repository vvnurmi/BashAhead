module BashAhead.Library

open Types

let weapons =
    let weaponsList = [
        { name = "bow"; power = 2<hp>; rangeMin = 3; rangeMax = 10 };
        { name = "sword"; power = 3<hp>; rangeMin = 2; rangeMax = 3 };
        { name = "fangs"; power = 1<hp>; rangeMin = 1; rangeMax = 2 };
    ]
    weaponsList |> List.map (fun w -> w.name, w) |> Map.ofList

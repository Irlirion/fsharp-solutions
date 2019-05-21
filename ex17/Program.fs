// 43.3
let try_find key m =
    try
        Some (Map.find key m)
    with
        | :? System.Collections.Generic.KeyNotFoundException -> None

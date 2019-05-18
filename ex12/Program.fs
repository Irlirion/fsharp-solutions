// 34.1
let upto =
    let rec iter_upto (n, i) =
        if i = n then [ i ]
        else i :: iter_upto (n, i + 1)
    function
 | n when n >= 1 -> iter_upto (n, 1)
 | _ -> []

// 34.2
let rec dnto = function
 | 1 -> [ 1 ]
 | n when n > 1 -> n :: dnto (n - 1)
 | _ -> []

// 34.3
let evenn =
    let rec iter_evenn (n, i) =
        if n = 1 then [ i ]
        else i :: iter_evenn (n - 1, i + 2)
    function
 | n when n >= 1 -> iter_evenn (n, 0)
 | _ -> []


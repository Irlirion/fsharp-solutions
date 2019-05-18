// 34.1
let upto =
    let rec iter_upto (n, i) =
        if i = n then [ i ]
        else i :: iter_upto (n, i + 1)
    function
 | n when n >= 1 -> Some(iter_upto (n, 1))
 | _ -> None

// 34.2
let rec dnto = function
 | 1 -> Some([ 1 ])
 | n when n > 1 -> Some(n :: Option.get (dnto (n - 1)))
 | _ -> None

// 34.3
let evenn =
    let rec iter_evenn (n, i) =
        if n = 1 then [ i ]
        else i :: iter_evenn (n - 1, i + 2)
    function
 | n when n >= 1 -> Some(iter_evenn (n, 0))
 | _ -> None


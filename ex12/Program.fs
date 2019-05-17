// 34.1
let upto = fun n ->
    let rec iter_upto (n, i) =
        if i = n then [i]
        else i :: iter_upto (n, i + 1)
    iter_upto (n, 1)

// 34.2
let rec dnto = fun n ->
    if n = 1 then [1]
    else n :: dnto (n - 1)

// 34.3
let evenn = fun n ->
    let rec iter_evenn (n, i) =
        if n = 1 then [i]
        else i :: iter_evenn (n - 1, i + 2)
    iter_evenn (n, 0)

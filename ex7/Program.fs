// 20.3.1
let vat n x = x + float n * x / 100.

// 20.3.2
let unvat n x = x * 100. / float (n + 100)

// 20.3.3
let rec min f =
    let rec iter_min (f, n) =
        if f (n) = 0 then n
        else iter_min (f, n + 1)
    iter_min (f, 0)

// 20.3.1
let vat n x = x + float n * x / 100.

// 20.3.2
let unvat n x = x * 100. /  float (n + 100)

// 20.3.3
let rec min f =
    let mutable n = 0
    while f(n) <> 0 do
        n <- n + 1
    n

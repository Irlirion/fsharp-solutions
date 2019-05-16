// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let prime n =
    let rec iter_prime (n, i) =
        if i >= n then true
        elif notDivisible (i, n) then
            false
        else iter_prime (n, i + 1)
    iter_prime (n, 2)

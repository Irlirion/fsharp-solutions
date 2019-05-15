// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let  prime  n =
    let mutable flag = true
    for i = 2 to n - 1 do
         if notDivisible(i, n) then
             flag <- false
    flag
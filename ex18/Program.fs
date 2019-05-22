// 47.4.1
let f n =
    let mutable mul = 1
    let g x = mul <- mul * x
    List.iter g [ 1..n ]
    mul

// 47.4.2
let fibo n =
    if n < 2 then 0
    else
        let mutable precious = 1
        let mutable current = 1
        let mutable next = 1
        for i = 3 to n - 1 do
            next <- current + precious
            precious <- current
            current <- next
        next

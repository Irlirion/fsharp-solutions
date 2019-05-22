// 47.4.1
let f n =
    let mutable mul = 1
    let g x = mul <- mul * x
    List.iter g [1 .. n]
    mul

// 47.4.2
let fibo n =
    let mutable fib1 = 0
    let mutable fib2 = 1
    let mutable fibonacci = fib1
    let f x =
        fibonacci <- fib1 + fib2
        fib1 <- fib2
        fib2 <- fibonacci
    List.iter f [2.. n]
    fibonacci

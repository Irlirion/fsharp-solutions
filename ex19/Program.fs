// 48.4.1
let rec fibo1 n n1 n2 =
    match n with
    | 0 -> n2
    | n -> fibo1 (n - 1) (n1 + n2) n1

// 48.4.2
let rec fibo2 c n =
    match n with
    | 0 -> c 0
    | 1 -> c 1
    | n -> fibo2 (fun f -> fibo2 (fun g -> c (f + g)) (n - 2)) (n - 1)

// 48.4.3
let rec bigList n k =
    match n with
    | 0 -> k []
    | n -> bigList (n - 1) (fun res -> k (1 :: res))

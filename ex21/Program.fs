// 50.2.1
let fac_seq =
    let rec fac m n =
        seq {
            yield m * n
            yield! fac (m * n) (n + 1)
        }
    seq { yield 1 ; yield 1 ; yield 2 ; yield! (fac 2 3) }

// 50.2.2
let seq_seq =
    let rec f n =
        seq {
            if n % 2 = 0 then yield (n / 2) else yield - (n + 1) / 2
            yield! f (n + 1)
        }
    seq { yield 0; yield -1; yield 1; yield! (f 3) }

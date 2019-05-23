// 50.2.1
let fac_seq n =
    let factorial n =
        let rec f x a =
            if x <= 1 then a
            else f (x - 1) (a * x)
        f n 1
    seq {
        for i in 0 .. n do
            yield factorial i
    }

// 50.2.2
let seq_seq n =
    let f n =
        match n with
        | n when n % 2 = 0 -> n / 2
        | n -> - (n + 1) / 2
    seq {
        for i in 0 .. n do
            yield f i
    }

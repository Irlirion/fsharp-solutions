// 50.2.1
let fac_seq =
    let factorial n =
        let rec f x a =
            if x <= 1 then a
            else f (x - 1) (a * x)
        f n 1
    fun n -> seq {
        for i in 0 .. n do
            yield factorial i
    }

// 50.2.2
let seq_seq =
    let f n =
        match n with
        | n when n % 2 = 0 -> n / 2
        | n -> - (n + 1) / 2
    fun n -> seq {
        for i in 0 .. n do
            yield f i
    }

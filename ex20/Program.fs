// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2 * i)

// 49.5.2
let fac_seq =
    let factorial n =
        let rec f x a =
            if x <= 1 then a
            else f (x - 1) (a * x)
        f n 1
    Seq.initInfinite (factorial)

// 49.5.3
let seq_seq =
    let f n =
        match n with
        | n when n % 2 = 0 -> n / 2
        | n -> - (n + 1) / 2
    Seq.initInfinite (f)
// 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | [ x ] when p x -> x
    | head :: tail when p head -> head + sum (p, tail)
    | _ :: tail -> 0 + sum (p, tail)
    | _ -> 0

// 40.2.1
let rec count (xs, n) =
    match xs with
    | head :: tail when head < n -> count (tail, n)
    | head :: _ when head > n -> 0
    | n :: tail -> 1 + count (tail, n)
    | [ n ] -> 1
    | [] -> 0

// 40.2.2
let rec insert (xs, n) =
    match xs with
    | head :: tail when head >= n -> n :: head :: tail
    | head :: tail when head < n -> head :: insert (tail, n)
    | _ -> n :: xs

// 40.2.3
let rec intersect (xs1: list<int>, xs2: list<int>) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> intersect (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> intersect (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: intersect (tail1, tail2)
    | ([ x ], [ y ]) when x = y -> [ x ]
    | _ -> []

// 40.2.4
let rec plus (xs1: list<int>, xs2: list<int>) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> head1 :: plus (tail1, xs2)
    | (head1 :: _, head2 :: tail2) when head1 > head2 -> head2 :: plus (xs1, tail2)
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> head1 :: head2 :: plus (tail1, tail2)
    | (_, []) -> xs1
    | ([], xs2) -> xs2
    | ([ x ], [ y ]) when x = y -> [ x; y ]
    | _ -> []

// 40.2.5
let rec minus (xs1: list<int>, xs2: list<int>) =
    match (xs1, xs2) with
    | (head1 :: tail1, head2 :: _) when head1 < head2 -> head1 :: minus (tail1, xs2)
    | (head1 :: _, head2 :: _) when head1 > head2 -> [ head1 ]
    | (head1 :: tail1, head2 :: tail2) when head1 = head2 -> minus (tail1, tail2)
    | (xs1, []) -> xs1
    | _ -> []

// 40.3.1
let rec smallest =
    let rec iter (xs: list<int>, min) =
        match xs with
        | [ x ] when x < min -> Some(x)
        | head :: tail when head < min -> iter (tail, head)
        | _ :: tail -> iter (tail, min)
        | [] -> Some(min)
    function
    | [] -> None
    | head :: tail -> iter (tail, head)

// 40.3.2
let rec delete (n, xs) =
    match xs with
    | head :: tail when head = n -> tail
    | head :: tail -> head :: delete (n, tail)
    | _ -> []


// 40.3.3
let rec sort = fun xs ->
    let min = smallest xs
    match (xs, min) with
    | (_ :: _, min) -> Option.get (min) :: sort (delete (Option.get (min), xs))
    | _ -> []

// 40.4
let rec revrev = fun (xs: list<list<int>>) ->
 match xs with
 | [] -> []
 | [ x ] -> [ List.rev x ]
 | head :: tail -> revrev tail @ [ List.rev head ]

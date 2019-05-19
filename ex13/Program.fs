// 39.1
let rec rmodd = function
 | _ :: (head2 :: tail) -> head2 :: rmodd tail
 | _ -> []

// 39.2
let rec del_even =
    let is_odd n = n % 2 <> 0
    function
    | [ x ] when is_odd x -> [ x ]
    | head :: tail when is_odd head -> head :: del_even tail
    | _ :: tail -> del_even tail
    | _ -> []

// 39.3
let multiplicity x xs =
    let rec iter (x, xs, i) =
        match xs with
        | [ e ] when e = x -> i + 1
        | [] -> i
        | e :: tail when e = x -> iter (x, tail, i + 1)
        | _ :: tail -> iter (x, tail, i)
        | _ -> 0
    iter (x, xs, 0)


// 39.4
let rec split =
    let rec even = function
    | [ x ] -> [ x ]
    | head :: (_ :: tail) -> head :: even tail
    | _ -> []

    let rec odd = function
    | [ _ ] -> []
    | _ :: (head2 :: tail) -> head2 :: odd tail
    | _ -> []

    fun xs -> (even xs, odd xs)

// 39.5
let rec zip xs1 xs2 =
    if List.length xs1 <> List.length xs2 then failwith "The lists are of different length"
    else match (xs1, xs2) with
         | ([ x ], [ y ]) -> [ (x, y) ]
         | (head1 :: tail1, head2 :: tail2) -> (head1, head2) :: zip tail1 tail2
         | _ -> []

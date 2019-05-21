// 42.3
let rec allSubsets n k =
    let rec powerset xs=
        match xs with
        | [] -> [[]]
        | head::tail -> List.fold (fun head1 tail1 -> (head::tail1)::tail1::head1) [] (powerset tail)
    Set.filter (fun xs -> Set.count xs = k) (Set.ofList (List.map Set.ofList (powerset [1 .. n])))
// 41.4.1
let list_filter f xs = List.foldBack (fun head tail -> if f head then head :: tail else tail) xs []

// 41.4.2
let sum (p, xs) = List.fold (+) 0 (List.filter p xs)

// 41.4.3
let revrev = fun lst -> List.fold (fun head tail -> List.rev tail :: head) [] lst

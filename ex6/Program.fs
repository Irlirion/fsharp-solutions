// 17.1
let rec pow = function
| (s, 1) -> s
| (s, n) -> s + pow(s, n - 1)

// 17.2
let rec isIthChar (s: string, n, c) = s.[n] = c

// 17.3
let rec occFromIth (s, n, c) =
    if n >= String.length s then 0
    else (if isIthChar(s, n, c) then 1 else 0) + occFromIth(s, (n + 1), c)  
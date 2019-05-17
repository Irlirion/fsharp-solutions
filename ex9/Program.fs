// 23.4.1
let all (gold, silver, copper) =
    gold * 240 + silver * 12 + copper

let get_copper all =
    all % 12

let get_silver all =
    int (float all / 12.) % 20

let get_gold all =
    int (float all / 240.)

let (.+.) x y =
    let sum_all (all1, all2) = all1 + all2
    let all = sum_all (all x, all y)
    (get_gold all, get_silver all, get_copper all)


let (.-.) x y =
    let sub_all (all1, all2) = all1 - all2
    let all = sub_all (all x, all y)
    (get_gold all, get_silver all, get_copper all)

// 23.4.2
let (.+) x y =
    let (r1, i1) = x
    let (r2, i2) = y
    (r1 + r2, i1 + i2)

let (.-) x y =
    let (r2, i2) = y
    x .+ (-r2, -i2)

let (.*) x y =
    let (r1, i1) = x
    let (r2, i2) = y
    (r1 * r2 - i1 * i2, i1 * r2 + r1 * i2)

let (./) x y =
    let (r2, i2) = y
    x .* (r2 / (r2 * r2 + i2 * i2), -i2 / (r2 * r2 + i2 * i2))
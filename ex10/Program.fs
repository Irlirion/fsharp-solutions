type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let h1 = if x.f = "AM" then x.hours else x.hours + 12
    let h2 = if y.f = "AM" then y.hours else y.hours + 12
    if h1 > h2 then true
    elif h1 = h2 && x.minutes > y.minutes then true
    else false

let t0 = { hours = 0; minutes = 0; f = "AM" }
let t1 = { hours = 0; minutes = 0; f = "PM" }
let t2 = { hours = 1; minutes = 0; f = "AM" }
let t3 = { hours = 0; minutes = 1; f = "AM" }
let t4 = { hours = 1; minutes = 0; f = "PM" }
let t5 = { hours = 0; minutes = 1; f = "PM" }

printfn "%b" (t0 .>. t1)
printfn "%b" (t2 .>. t0)
printfn "%b" (t3 .>. t0)
printfn "%b" (t4 .>. t2)
printfn "%b" (t5 .>. t4)
printfn "%b" (t0 .>. t4)
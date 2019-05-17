type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y =
    let h1 = if x.f = "AM" then x.hours else x.hours + 12
    let h2 = if y.f = "AM" then y.hours else y.hours + 12
    if h1 > h2 then true
    elif h1 = h2 && x.minutes > y.minutes then true
    else false

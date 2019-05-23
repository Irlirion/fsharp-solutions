type 'a cell = Nil | Cons of 'a * Lazy<'a cell>

let hd (s : 'a cell) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

let tl (s : 'a cell) : Lazy<'a cell> =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g


// 51.3
let rec nth (s : 'a cell) (n : int) : 'a = if n = 0 then hd s else nth ((tl s).Force()) (n - 1)
  

// например, получить 30000-й элемент:
// nth n0 30000

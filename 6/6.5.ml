type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec comptree x n =
    if n = 0 then Lf else Br (x, comptree x (n-1), comptree x (n-1))

let rec comptree' n =
    let rec _comptree' n i =
        if n = 0 then Lf else Br (i, _comptree' (n-1) (i*2), _comptree' (n-1) (i*2+1))
in _comptree' n 1
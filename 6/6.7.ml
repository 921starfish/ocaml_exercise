type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec refrect t =
    match t with
    Lf -> Lf
  | Br (x,left,right) -> Br (x, refrect(right), refrect(left))
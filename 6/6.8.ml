type 'a tree = Lf | Br of 'a * 'a tree * 'a tree
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list

let rec sibling t =
  match t with
    Lf -> []
  | Br(x, left, right) -> (Br(x, left, Lf)::sibling(right))

let rec rtree_of_tree rt =
  match rt with
    Lf -> RLf
  | Br(None, _, _) -> RLf
  | Br(Some x, left, right) -> RBr(x, (List.map rtree_of_tree (sibling left)))

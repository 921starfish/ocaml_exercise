type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 教科書のpreord *)
let rec preord t l =
    match t with
    Lf -> l
  | Br(x, left, right) -> x :: (preord left (preord right l))

let rec inord t l =
    match t with
    Lf -> l
  | Br(x, left, right) -> (inord left (x :: inord right l))

let rec postord t l =
    match t with
    Lf -> l
  | Br(x, left, right) -> (postord left (postord right (x :: l)))
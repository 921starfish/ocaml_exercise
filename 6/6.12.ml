type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec add key tr =
  match tr with
    Lf -> Br (key, Lf, Lf)
  | Br (key', left, right) -> 
    if key = key' then 
      Br(key', left, right)
    else if key < key' then 
      Br(key', add key left, right)
    else 
      Br(key', left, add key right)

let ( << ) tr key = add key tr;;

Lf << 1 << 2 << 3 << 4;;
Lf << 1 << 2 << 4 << 3;;
Lf << 1 << 3 << 2 << 4;;
Lf << 1 << 3 << 4 << 2;;
Lf << 1 << 4 << 2 << 3;;
Lf << 1 << 4 << 3 << 2;;

Lf << 2 << 1 << 3 << 4;;
Lf << 2 << 1 << 4 << 3;;
Lf << 2 << 3 << 1 << 4;;
Lf << 2 << 3 << 4 << 1;;
Lf << 2 << 4 << 1 << 3;;
Lf << 2 << 4 << 3 << 1;;

Lf << 3 << 1 << 2 << 4;;
Lf << 3 << 1 << 4 << 2;;
Lf << 3 << 2 << 1 << 4;;
Lf << 3 << 2 << 4 << 1;;
Lf << 3 << 4 << 1 << 2;;
Lf << 3 << 4 << 2 << 1;;

Lf << 4 << 1 << 2 << 3;;
Lf << 4 << 1 << 3 << 2;;
Lf << 4 << 2 << 1 << 3;;
Lf << 4 << 2 << 3 << 1;;
Lf << 4 << 3 << 1 << 2;;
Lf << 4 << 3 << 2 << 1;;
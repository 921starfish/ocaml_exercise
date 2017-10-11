let rec max_list lst = match lst with
    x :: [] -> x
  | x :: rest -> let y = max_list rest in if x > y then x else y
  | [] -> [];;
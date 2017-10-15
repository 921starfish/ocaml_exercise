let rec filter f lst =
    match lst with
    [] -> []
  | x :: rest -> if f x then x :: (filter f rest) else filter f rest;;
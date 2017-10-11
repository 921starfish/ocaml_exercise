let rec filter f = function
    [] -> []
  | x :: rest when f x -> x :: filter f rest
  | x :: rest -> filter f rest;;
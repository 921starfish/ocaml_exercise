let rec nested_length lst = 
    match lst with
    [] -> 0
  | x :: rest -> (List.length x) + nested_length rest;;
let rec unzip lst =
    match lst with
    [] -> ([], [])
  | (a, b) :: rest -> (a::(fst (unzip rest)), b::(snd (unzip rest)));;
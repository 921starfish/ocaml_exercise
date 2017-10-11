let rec unzip lst = match lst with
    [] -> ([], [])
  | (a, b) :: rest -> let (arest, brest) = unzip rest in (a :: arest, b :: brest);;
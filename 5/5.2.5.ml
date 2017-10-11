let rec zip lst1 lst2 =
  match (lst1, lst2) with
  | (a :: arest, b :: brest) -> (a, b) :: zip arest brest
  | (_, _) -> [];;
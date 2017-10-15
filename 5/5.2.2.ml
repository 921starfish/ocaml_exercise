let rec roman def n =
    match def with
    [] -> ""
  | (x, str) :: rest -> if n >= x then str ^ roman def (n - x) else roman rest n;;
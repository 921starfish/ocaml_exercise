let rec roman def n =
  match def with
    [] -> ""
  | (n', s) :: rest when n >= n' -> s ^ roman def (n - n')
  | (n', s) :: rest -> roman rest n;;
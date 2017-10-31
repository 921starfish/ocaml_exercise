type nat = Zero | OneMoreThan of nat

let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n)

let rec monus m n =
    match (m, n) with
    (m', Zero) -> m'
  | (Zero, _) -> Zero
  | (OneMoreThan m',OneMoreThan n') -> if m' = n' then Zero else (monus m' n')

let rec mul m n =
    match (m, n) with
    (_, Zero) -> Zero
  | (Zero, _) -> Zero
  | (OneMoreThan m',n') -> (add (mul m' n') n')

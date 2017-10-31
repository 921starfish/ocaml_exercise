type nat = Zero | OneMoreThan of nat

let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan one
let three = OneMoreThan two
let four = OneMoreThan three

let rec monus m n =
    match (m, n) with
    (m', Zero) -> Some m'
  | (Zero, _) -> None
  | (OneMoreThan m',OneMoreThan n') -> if m' = n' then Some Zero else (monus m' n')
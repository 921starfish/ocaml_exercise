type arith = Const of int | Add of arith * arith | Mul of arith * arith

let rec string_of_arith expression =
  match expression with
    Const c -> string_of_int c
  | Add (m, n) -> "("^string_of_arith m^"+"^string_of_arith n^")"
  | Mul (m, n) -> "("^string_of_arith m^"*"^string_of_arith n^")"

let rec expand expression =
  match expression with
    Const c -> Const c
  | Mul (Add(m, n), a) -> Add(expand(Mul(m, a)), expand(Mul(n, a)))
  | Mul (a, Add(m, n)) -> Add(expand(Mul(a, m)), expand(Mul(a, n)))
  | Add (m, n) -> Add (expand m, expand n)
  | Mul (m, n) -> Mul (expand m, expand n)

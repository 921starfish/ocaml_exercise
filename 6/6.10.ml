type arith = Const of int | Add of arith * arith | Mul of arith * arith

let rec eval expression =
  match expression with
    Const c -> c
  | Add (m, n) -> eval m + eval n
  | Mul (m, n) -> eval m * eval n
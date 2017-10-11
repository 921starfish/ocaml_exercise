let nextrand seed =
  let a = 16807.0 and m = 2147483647.0 in
  let t = a *. seed
  in t -. m *. floor ( t/. m);;

let rec randlist n seed tail =
  if n = 0 then (seed, tail)
  else randlist (n - 1) (nextrand seed) (seed::tail);;

(*以上準備*)

let rec at_quick_sort = function
    ([] | [_]) as l -> l
  | pivot :: rest ->
    let rec partition left right = function
        [] ->
        (at_quick_sort left) @ (pivot :: at_quick_sort right)
      | y::ys ->
        if pivot < y then partition left ( y::right) ys
        else partition (y :: left) right ys
    in partition [] [] rest;;

(*以上@を使うクイックソート*)

(*以下演習の解答*)

let rec quick_sort lst results =
  match lst with
    [] -> results
  | [x] -> x::results
  | pivot :: rest ->
    let rec partition left right lst results =
      match lst with
        [] -> 
        quick_sort left (pivot :: quick_sort right results)
      | y::ys -> 
        if pivot < y then partition left (y::right) ys results
        else partition (y::left) right ys results 
    in partition [] [] rest results;;

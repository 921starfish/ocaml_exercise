let rec repeat f n x =
  if n > 0 then repeat f (n - 1) (f x) else x;;

let fib n =
  let (fibn, _) = repeat (fun (x, y) -> (x + y, x)) n (0, 1)
  in fibn;;
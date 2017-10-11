let rec pow x n =
  if n == 1 then x else pow x (n-1) *. x;;

let rec pow n x =
  if n == 1 then x
  else pow (n - 1) x *. x;;
let cube = pow 3;;

let rec pow x n =
  if n == 1 then x else pow x (n-1) *. x;;
let cube = (fun x -> pow x 3);;
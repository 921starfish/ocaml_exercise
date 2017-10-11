(*比較用*)
let rec map f = function
    [] -> []
  | x :: rest -> f x :: map f rest;;


let map2 f lst =
  let results = List.fold_left (fun x y -> (f y)::x) [] lst in
    List.fold_left (fun x y -> y::x) [] results;;

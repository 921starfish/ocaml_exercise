let f x = x + 1;;
let g x = x * 2;;
let l = [1;2;3];;
List.map f (List.map g l);;
(*以上デバッグ用*)

List.map (fun x -> f (g x)) l;;
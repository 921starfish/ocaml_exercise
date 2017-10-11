let rec take n = function
    x :: rest when n > 0 -> x :: take (n - 1) rest
  | _ -> [];;


let rec drop n = function
    x :: rest when n > 0 -> drop (n - 1) rest
  | l -> l;;
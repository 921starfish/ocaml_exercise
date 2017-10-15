let rec take n lst =
    match lst with
    x :: rest when n > 0 -> x :: take (n - 1) rest
  | _ -> [];;


let rec drop n lst =
    match lst with
    x :: rest when n > 0 -> drop (n - 1) rest
  | l -> l;;
let rec concat lst =
  List.fold_right (@) lst [];;

let rec forall f lst =
  List.fold_right (fun x -> (&&) (f x)) lst true;;

let rec exists f lst =
  List.fold_right (fun x -> (||) (f x)) lst false;;
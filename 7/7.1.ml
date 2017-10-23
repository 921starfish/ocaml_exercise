let rec find x lst = 
  let rec find' x lst = match lst with
      [] -> raise Not_found
    | a :: rest when a = x -> 1
    | _ :: rest -> 1 + find' x rest
  in try Some (find' x lst) with Not_found -> None
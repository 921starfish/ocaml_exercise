let max_list lst = 
    let rec _max_list lst max =
        match lst with
      [] -> max
    | x::rest -> if x > max then _max_list rest x else _max_list rest max
in _max_list lst min_int;;

let rec pos (n: int) : float =
    if n < 0 then 0.0
    else pos(n-1) +. 1.0 /. (float_of_int (4 * n + 1)) -. 1.0 /. (float_of_int (4 * n + 3)) ;;
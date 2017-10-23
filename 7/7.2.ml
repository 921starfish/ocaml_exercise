exception IsZero

let rec prod_list lst = 
  let rec prod_list' lst results = match lst with
      [] -> results
    | a::rest -> if a = 0 then raise IsZero else prod_list' rest a*results
  in try (prod_list' lst 1) with IsZero -> 0;;
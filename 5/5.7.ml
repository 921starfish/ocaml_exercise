let squares r = 
  let rec rec_squares r x y = 
  if x < 0 then [] else
    if y < 0 then rec_squares r (x-1) (x-1) else
      if (x * x) + (y * y) = r then (x,y)::rec_squares r x (y-1) else rec_squares r x (y-1)
in rec_squares r (int_of_float(sqrt (float_of_int r))) (int_of_float(sqrt (float_of_int r)));;
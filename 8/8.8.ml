let array_iteri f ary = 
  for i = 0 to Array.length ary - 1 do
    f (i+1) ary.(i)
  done
let array_iter f ary = 
  for i = 0 to Array.length ary - 1 do
    f ary.(i)
  done
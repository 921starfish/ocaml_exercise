let pow(x, n) =
  let rec iterpow(iterator,x,n,result) =
    	if iterator == n then result
    else iterpow(iterator + 1, x, n, x*.result)
  in iterpow(1, x, n, x);;
let rec fr i to_or_downto bound body =
  if to_or_downto = "to" then
    begin
      if !i <= bound then
        begin body (); i := !i + 1; fr i to_or_downto bound body end
    end 
  else if to_or_downto = "downto" then 
    begin 
      if bound <= !i then
        begin body (); i := !i - 1; fr i to_or_downto bound body end 
    end;;

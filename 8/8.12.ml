let cp fn1 fn2 =
  let ip = open_in fn1 and op = open_out fn2 in
  try 
    while true do
      output_string op ((input_line ip)^"\n");
    done;
  with End_of_file -> ();
    flush op;close_in ip;close_out op
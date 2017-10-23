let display_file filename =
  let ip = open_in filename and i = ref 1 in
  try 
    while true do
      print_endline (string_of_int !i^" "^(input_line ip));
      i := !i+1;
    done;
  with End_of_file -> ();
    close_in ip
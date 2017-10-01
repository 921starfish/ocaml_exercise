let usd_to_string (dollar: float) : string = 
	string_of_float(dollar) ^ " dollars are " ^ string_of_int(int_of_float (dollar *. 114.32)) ^ " yen";; 
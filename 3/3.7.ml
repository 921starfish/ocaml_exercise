let rec pow (x,n) =
  	if n == 1 then x else pow(x,n-1) *. x;;

let rec pow (x,n) =
  	if n == 1 then x else 
    	let t = pow(x, n / 2) in
    	if n mod 2 == 0 then t *. t else t *. t *. x;;

(*①*)
let rec gcd(m, n) =
    if m mod n = 0 then n
else gcd(n, m mod n);;

(*②*)
let rec comb(n, m) =
    if m = 0 || m = n then 1
    else comb(n - 1, m) + comb(n - 1, m - 1);;

(*③*)
let fib n =
    let rec iterfib(n, a, b) =
        if n = 1 then a
        else iterfib(n-1, b, a+b) in
    iterfib(n, 1, 1);;

(*④*)
let max_ascii str =
    let rec itermax_ascii (i, str, max) = 
        if i = String.length str - 1 then max
        else itermax_ascii(i+1,str,if str.[i]>max then str.[i] else max) in
itermax_ascii(0,str,char_of_int 0);;
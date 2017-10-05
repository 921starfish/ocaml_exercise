(*①*)
let geo_mean (x,y) = sqrt (x*.y);;

(*②*)
let bmi (name,shinchou,taijuu) =
	let a = taijuu /. (shinchou *.shinchou) in
	if a < 18.5 then name^"さんはやせています" else
	if a < 25.0 then name^"さんは標準です" else 
	if a < 30.0 then name^"さんは肥満です" else name^"さんは高度肥満です";;

(*③*)
let sum_and_diff (x, y) = (x + y, x - y);;(*debug用*)
let f (x,y) = ((x + y) / 2,(x - y) / 2);;
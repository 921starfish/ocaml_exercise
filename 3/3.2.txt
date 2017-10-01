(* b_1 && b_2 *)

if b_1 == true 
	then
		if b_2 == true then true else false
	else false;;

(* ifを1回しか用いない方法 *)
if b_1 == true then b_2 else false;;

(* b_1 || b_2 *)

if b_1 == false
	then
		if b_2 == false then false else true
	else true;;

(* ifを1回しか用いない方法 *)
if b_1 == false then b_2 else true;;
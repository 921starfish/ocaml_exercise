(* int -> int -> int -> int *)
(* int型を受け取ると「int型を受け取ると『int型を受け取ってint型を返す関数』を返す関数」を返す関数*)
let f1 a b c = a + b + c;;

(* (int -> int) -> int -> int *)
(* 『int型を受け取ってint型を返す関数』とint型を受け取って、int型を返す関数*)
let f1 (f2:int -> int) a = f2 a+1;;

(* (int -> int -> int) -> int *)
(* 「int型を受け取ると『int型を受け取ってint型を返す関数』を返す関数」を受け取って、int型を返す関数*)
let f1 (f2:int -> int -> int) = f2 1 2;;
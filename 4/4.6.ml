let k x y = x;;
let s x y z = x z (y z);;
(*以上、準備*)

let f x y = k (s k k) x y;;
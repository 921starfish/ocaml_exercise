type 'a ref = { mutable contents : 'a }

(* 関数 ref *)
let ref a = { contents = a}

(* 前置演算子 ! *)
let (!) x = x.contents

(* 中置演算子 := *)
let (:=) x y = x.contents <- y
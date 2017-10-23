let f = ref (fun y -> y + 1)
let funny_fact x = if x = 1 then 1 else x * (!f (x - 1));;

f := funny_fact;;

funny_fact 5;;

(*
fはint型を受け取ってint型を返す関数をcontentsとしてもつことができるref型の変数として定義される
funny_factはint型の引数xを受け取って、x=1なら「1」をそれ以外なら「x * (!f (x - 1))」を返す関数である

ここで、f の参照先を funny_fact にすると、!fはfunny_fact自身を呼び出すため、recを使わずに再帰を実現し、階上が計算される
*)

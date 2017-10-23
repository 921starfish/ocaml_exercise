(* while *)
let fib n = if n = 1 then 1 else 
    let i = ref 0 and f0 = ref 0 and f1 = ref 1 and fn = ref 0 in
    while (!i < n-1) do
      fn := !f0 + !f1;
      f0 := !f1;
      f1 := !fn;
      i:=!i + 1
    done;
    !fn


(* for *)
let fib n = if n = 1 then 1 else 
    let f0 = ref 0 and f1 = ref 1 and fn = ref 0 in
    for i = 0 to n-2 do
      fn := !f0 + !f1;
      f0 := !f1;
      f1 := !fn
    done;
    !fn
let integral f a b = 
  let d = (b-.a)/.10000.0 in (* 仮にδを与えられた幅を10000分割した時の大きさとした *)
      let rec trapezoidal f a b =
        if a >= b then 0.0 else (f a +. f a+.d) *. d /. 2.0 +. trapezoidal f (a+.d) b
  in trapezoidal f a b;;
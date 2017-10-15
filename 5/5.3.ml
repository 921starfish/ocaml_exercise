(*①*)
let rec mem a s =
    match s with
    []->false
  |x::rest when a == x ->true
  |x::rest -> mem a rest;;

(*②*)
let rec intersect s1 s2 =
    match (s1, s2) with
    ([], _) -> []
  | (x::s1rest, s2rest) when mem x s2rest -> x::(intersect s1rest s2rest)
  | (x::s1rest, s2rest) -> intersect s1rest s2rest;;


(*③*)
let union s1 s2 =
  let intersection = intersect s1 s2 in(*共通部分を先に求める*)
  let rec _union s1 wa = 
      match s1 with
      [] -> s2(*s1が空ならs2*)
    |x::s1rest when mem x wa -> _union s1rest wa
    |x::s1rest -> x::(_union s1rest wa) in
  _union s1 intersection;;


(*④*)
let diff s1 s2 =
  let intersection = intersect s1 s2 in(*共通部分を先に求める*)
  let rec _diff s1 wa =
      match s1 with
      [] -> [](*s1が空なら空*)
    |x::s1rest when mem x wa -> _diff s1rest wa
    |x::s1rest -> x::(_diff s1rest wa) in
  _diff s1 intersection;;

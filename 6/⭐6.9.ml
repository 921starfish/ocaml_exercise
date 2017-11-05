open List


type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list
type token = PCDATA of string | Open of string | Close of string| LST of (string,string)xml

let sub lst start len = Array.to_list(Array.sub (Array.of_list lst) start len)

let rec drop n lst =
  match lst with
    x :: rest when n > 0 -> drop (n - 1) rest
  | l -> l

let rec xml_of_tokens' lst  =
  match lst with
    [] -> []
  | Open x::rest -> [XBr(x,xml_of_tokens' rest)]
  | PCDATA x::rest -> XLf(Some x)::xml_of_tokens' rest
  | LST x::rest -> x::xml_of_tokens' rest
  | Close x::rest -> [];;

let rec bunkatsu lst i lo stack =
  match lst with
    [] -> stack
  | Open x::rest -> bunkatsu rest (i+1) (i::lo) (Open x::stack)
  | PCDATA x::rest -> bunkatsu rest (i+1) lo (PCDATA x::stack)
  | LST x::rest -> bunkatsu rest (i+1) lo (LST x::stack)
  | Close x::rest -> bunkatsu rest (hd lo+1) 
                       (tl lo) 
                       ((LST (hd ((xml_of_tokens' (rev (sub stack 0 (i-hd lo)))))))::(drop (i-hd lo) stack))


(*
(xml_of_tokens' (rev (sub stack 0 (hd lo))))
*)



(*
((LST (hd (xml_of_tokens' (rev (sub stack 0 (hd lo))))))::(drop (hd lo) stack))
*)



(* 目標
   XBr ("a",
   [XBr ("b", [XBr ("b", [XLf (Some "Hello1")])]);
   XBr ("c", [XLf (Some "Hello")])])
*)


let token_list = [Open "a"; Open "b"; Open "b"; PCDATA "Hello1"; Close "b"; Close "b";
                  Open "c"; PCDATA "Hello"; Close "c";Close "a"];;


#trace xml_of_tokens';;
#trace bunkatsu;;
bunkatsu token_list 0 [] [];;
(*closeを先に見つけてやる*)
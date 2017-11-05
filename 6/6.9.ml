open List

type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list
type token = PCDATA of string | Open of string | Close of string

type etoken = PCDATA of string | Open of string | Close of string | LST of (string,string)xml

let sub lst start len = Array.to_list(Array.sub (Array.of_list lst) start len)

let rec drop n lst =
  match lst with
    x :: rest when n > 0 -> drop (n - 1) rest
  | l -> l

let etoken_of_token (tkn:token):etoken = 
  match tkn with
    Open x -> Open x
  | PCDATA x -> PCDATA x
  | Close x -> Close x

let rec xml_of_tokens' lst  =
  match lst with
    [] -> []
  | Open x::[] -> [XBr(x,[XLf None])]
  | Open x::rest -> [XBr(x,xml_of_tokens' rest)]
  | PCDATA x::rest -> XLf(Some x)::xml_of_tokens' rest
  | LST x::rest -> x::xml_of_tokens' rest
  | Close x::rest -> []

let rec xml_of_etokens lst i lo stack =
  match lst with
    [] -> hd stack
  | Open x::rest -> xml_of_etokens rest (i+1) (i::lo) (Open x::stack)
  | PCDATA x::rest -> xml_of_etokens rest (i+1) lo (PCDATA x::stack)
  | LST x::rest -> xml_of_etokens rest (i+1) lo (LST x::stack)
  | Close x::rest -> xml_of_etokens rest (hd lo+1) 
                       (tl lo) 
                       ((LST (hd ((xml_of_tokens' (rev (sub stack 0 (i-hd lo)))))))::(drop (i-hd lo) stack))

let xml_of_tokens lst = 
  let f x = 
    match x with 
      LST a -> a 
    | _ -> raise (Failure "") 
  in f(xml_of_etokens (map etoken_of_token lst) 0 [] [])
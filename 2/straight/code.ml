type suit = S | H | D | C;;
type card = Card of int * suit;;

let getIntCard a = match a with Card(v, _) -> v;;

let straight (a, b, c, d, e) = 
  match (a, b, c, d, e) with
| _ -> getIntCard a <= getIntCard b && getIntCard b <= getIntCard c && getIntCard c <= getIntCard d && getIntCard d <= getIntCard e;;
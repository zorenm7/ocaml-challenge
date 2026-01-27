type suit = S | H | D | C;;
type card = Card of int * suit;;

let rndSuit () = match Random.int(4) with
| 0 -> S
| 1 -> H
| 2 -> D
| _ -> C
;;

let getSuitCard a = match a with Card(_, s) -> s;;
let getIntCard a = match a with Card(v, _) -> v;;

let checkDifferentSuits a b c d = getSuitCard a != getSuitCard b && getSuitCard b != getSuitCard c && getSuitCard c != getSuitCard d;;

let rndHand = ( Card(Random.int(10), rndSuit()), Card(Random.int(10), rndSuit()), Card(Random.int(10), rndSuit()), Card(Random.int(10), rndSuit()), Card(Random.int(10), rndSuit()));;

let poker (a, b, c, d, e) = match (a, b, c, d, e) with
| _ when getIntCard a = getIntCard b && getIntCard a = getIntCard c && getIntCard a = getIntCard d -> checkDifferentSuits a b c d
| _ when getIntCard a = getIntCard b && getIntCard a = getIntCard c && getIntCard a = getIntCard e -> checkDifferentSuits a b c e
| _ when getIntCard a = getIntCard b && getIntCard a = getIntCard d && getIntCard a = getIntCard e -> checkDifferentSuits a b d e
| _ when getIntCard a = getIntCard e && getIntCard a = getIntCard c && getIntCard a = getIntCard d -> checkDifferentSuits a e c d
| _ when getIntCard e = getIntCard b && getIntCard e = getIntCard c && getIntCard e = getIntCard d -> checkDifferentSuits e b c d
| _ -> false
;;
type card = Joker | Val of int;;

let highest_card p d = p > d;;

let win p d =
  match p, d with 
  | Joker, Joker -> false
  | _, Joker -> false
  | Joker, _ -> true 
  | _ -> highest_card p d
;;
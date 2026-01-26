let xor a b =
  (a || b) && not (a && b)
;;

let xor a b = 
  if a then not b else b
;;

let xor a b = 
  match a, b with
  | true, true -> false
  | false, false -> false
  | _ -> true
;;

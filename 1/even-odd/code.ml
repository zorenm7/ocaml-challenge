let is_even x = x mod 2 = 0;;

let correct x = x >= 1 && x <= 5;;

let win a b = 
  match correct a, correct b with
  | false, false -> 0
  | false, true -> -1
  | true, false -> 1
  | true, true when is_even (a+b) -> 1
  | _ -> -1
;;

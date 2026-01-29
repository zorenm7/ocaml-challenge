type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring b = 
  match b with
  | E -> ""
  | Z s -> "0" ^ string_of_bitstring s
  | U s -> "1" ^ string_of_bitstring s
;;

let rec len b = 
  match b with
  | E -> 0
  | Z s | U s -> 1 + len s
;;

let rec countU b = 
  match b with
  | E -> 0
  | Z s -> countU s
  | U s -> 1 + countU s
;;

let rec countZ b = 
  match b with
  | E -> 0
  | Z s -> 1 + countZ s
  | U s -> countZ s
;;

let rec concat a b = 
  match a with
  | E -> b
  | Z s -> Z (concat s b)
  | U s -> U (concat s b)
;;

let rec equals s1 s2 = 
  match s1, s2 with
  | E, E -> true
  | Z a, Z b | U a, U b -> equals a b
  | _, _ -> false
;;

let tl b = 
  match b with
  | E -> E
  | Z s -> s
  | U s -> s
;;

let rec prefix s1 s2 = 
  match s1, s2 with
  | E, E -> true
  | E, _ -> true
  | Z a, Z b | U a, U b -> prefix a b
  | _, _ -> false
;;

let rec substring s1 s2 =
  match s1, s2 with
  | E, _ -> true
  | _, E -> false
  | _, _ -> prefix s1 s2 || substring s1 (tl s2)
;; 
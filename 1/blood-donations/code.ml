type blood_group = A | B | AB | O;;

let check_groups x y =
  match x with
  | A -> y = A || y = AB
  | B -> y = B || y = AB
  | AB -> y = AB
  | O -> y = O || y = A || y = B || y = AB
;;
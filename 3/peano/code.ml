type nat = Z | S of nat

let rec iseven x = 
  match x with
  | Z -> true
  | S Z -> false
  | S (S a) -> iseven a
;;

let rec halve x = 
  match x with
  | Z -> Z
  | S Z -> Z
  | S (S a) -> S (halve a)
;;

let rec add x y = 
  match x with
  | Z -> y
  | S a -> S (add a y)
;;

let rec mul x y = 
  match x with
  | Z -> Z
  | S a -> add y (mul a y)
;;

let rec equals x y = 
  match x, y with
  | Z, Z -> true
  | Z, S _ -> false
  | S _, Z -> false
  | S a, S b -> equals a b
;;

let rec leq x y = 
  match x, y with
  | Z, Z -> true
  | Z, S _ -> true
  | S _, Z -> false
  | S a, S b -> leq a b
;;
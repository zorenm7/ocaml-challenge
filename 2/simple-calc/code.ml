type operation = Add of int * int | Sub of int * int | Mul of int * int | Div of int * int

let eval x = 
  match x with
  | Add (a,b) -> Some (a + b)
  | Sub (a,b) -> Some (a - b)
  | Mul (a,b) -> Some (a * b)
  | Div (_,0) -> None
  | Div (a,b) -> Some (a / b)
;;

let make_operation c a b =
  match c with
  | '+' -> Add (a,b)
  | '-' -> Sub (a,b)
  | '*' -> Mul (a,b)
  | '/' ->
      if b = 0 then failwith "Division by zero"
      else Div (a,b)
  | _ -> failwith "Unknown operation"
;;

assert (eval (Add (3,4)) = Some 7);;
assert (eval (Sub (10,3)) = Some 7);;
assert (eval (Mul (3,5)) = Some 15);;
assert (eval (Div (10,2)) = Some 5);;
assert (eval (Div (10,0)) = None);;

assert (eval (make_operation '+' 2 3) = Some 5);;
assert (eval (make_operation '-' 7 4) = Some 3);;
assert (eval (make_operation '*' 3 4) = Some 12);;
assert (eval (make_operation '/' 8 2) = Some 4);;
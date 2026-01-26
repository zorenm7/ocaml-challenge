let total_score (res : string * (int * int)) : int =
  let (n, (w, o)) = res in w + o
;;

let max2 a b = if a > b then a else b;;

let passed (res : string * (int * int)) : bool =
  let (n, (w, o)) = res in w >= 18 && o >=18;;

let string_of_result (n, (w, o)) = if passed(n, (w, o)) then n ^ "passed with total" ^ string_of_int(total_score(n, (w, o))) else n ^ "not passed";;

let merge (res1 : string * (int * int)) (res2 : string * (int * int)) : string * (int * int) = 
  let (n1, (w1, o1)) = res1 in let (n2, (w2, o2)) = res2 in 
  match n1 with
  | n2 -> (n1, (max2 w1 w2, max2 o1 o2))
  | _ -> failwith "Not the same student"
;;
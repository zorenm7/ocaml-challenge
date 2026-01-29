let rec mem x s = 
  match s with
  | [] -> false
  | [h] -> if h = x then true else false
  | h :: t -> if h = x then true else mem x t
;;

let rec subseteq xl yl = 
  match xl with
  | [] -> true
  | h :: t -> if mem h yl then subseteq t yl else false
;;

let rec seteq xl yl = 
  match xl with
  | [] -> true
  | h :: t -> if mem h yl then subseteq t yl else false
;;

let seteq xl yl = (List.length xl = List.length yl) && subseteq xl yl;;

let rec dup l = 
  match l with
  | [] -> false
  | h :: t -> if mem h t then true else dup t
;;

let rec mkset l = 
  match l with
  | [] -> []
  | h :: t -> if mem h t then mkset t else h :: mkset t
;;

let union a b = mkset (a @ b);;

let rec inter a b = 
  match a with
  | [] -> []
  | h :: t -> if mem h b then h :: inter t b else inter t b
;;

let rec diff a b = 
  match a with
  | [] -> []
  | h :: t -> if mem h b then diff t b else h :: diff t b
;;

let dsum xl yl = let l = xl @ yl in 
match l with
| [] -> []
| h :: t -> List.fold_left(fun acc x -> if mem x xl then (0, x) :: acc else (1, x) :: acc) [] l
;;

let rec powset xl = 
  match xl with
  | [] -> [[]]
  | h :: t -> powset t @ (List.map (fun subset -> h :: subset) (powset t))
;;
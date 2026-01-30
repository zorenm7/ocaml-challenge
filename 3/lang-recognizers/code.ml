let rec mem x l = match l with
| [] -> false
| h :: t -> (h = x) || (mem x t);;

let rec count0 = function
| [] -> 0
| h :: t -> (if h = 0 then 1 else 0) + count0 t;;

let rec count2 = function
| [] -> 0
| h :: t -> (if h = 2 then 1 else 0) + count2 t;;

let rec count1 = function
| [] -> 0
| h :: t -> (if h = 1 then 1 else 0) + count1 t;;

let rec count11 l = match l with
| [] -> false
| [n] -> false
| h :: x :: t -> (h = 1 && x = 1) || count11 (x :: t);;

let rec l0 w = 
  match w with
  | [] -> true
  | [h] -> h <> 0
  | h :: t -> l0 t
;;

let rec l1 w = 
  match w with
  | [] -> true
  | h :: t -> if h <> 0 then l1 t else if mem h t then false else true
;;

let rec l2 w = 
  match w with
  | [] -> true
  | h :: t -> if h <> 1 then l2 t else if mem 0 t then false else true
;;

let rec l3 w =
  match w with
  | [] -> true
  | h :: t -> if h <> 0 then l3 t else count11 t
;;

let rec l4 w = count0 w >= count1 w;;

let rec l5 w = count0 w = count1 w;;

let rec supp6 n b w = 
  match w with
  | [] -> n = 0 && b = 1
  | 0 :: t -> if b = 1 then false else supp6 (n + 1) 0 t
  | 1 :: t -> if n = 0 then false else supp6 (n - 1) 1 t
  | _ -> false
;;

let l6 w = w = [] || supp6 0 0 w;;

let rec l7 n = function
| [] -> true
| 0 :: t -> l7 (n + 1) t
| 1 :: t -> n = count0 t
| _ -> false;;

let rec l8 = function
| [] -> true
| 0 :: t -> l8 t
| 1 :: t -> if count2 t = count0 t then l8 t else false
| 2 :: t -> l8 t
| _ -> false;;

let rec l8 = function
| [] -> true
| h :: t -> if h = 1 then
      if count2 t = count0 t then l8 t 
      else false
    else l8 t
;;

let rec l9_aux count = function
  | [] -> count = 0
  | 0 :: t when count >= 0 -> l9_aux (count + 1) t
  | 1 :: t when count > 0  -> if mem 0 t then false else l9_aux (count - 1) t
  | _ -> false
;;

let l9 w = l9_aux 0 w;;


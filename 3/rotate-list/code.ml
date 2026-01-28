let rec rotate n l = 
  if n <= 0 then l 
  else match l with
  | [] -> []
  | h :: t -> rotate (n-1) (t @ [h]) 
;;
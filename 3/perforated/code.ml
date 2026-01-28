let rec is_perforated l = 
  match l with
  | [] -> true
  | [h] -> true
  | h :: m :: t -> if h + 2 <= m || h - 2 >= m then is_perforated (m :: t) else false
;;
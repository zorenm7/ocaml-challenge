let consecutive_even l = 
  match l with
  | [] -> 0
  | [h] -> if h mod 2 = 0 then 1 else 0
  | _ -> fst (List.fold_left(fun (best, current) x -> if x mod 2 = 0 then (max best (current + 1), current + 1) else (best, 0)) (0, 0) l)
;;
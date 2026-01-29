let rec compare_list cmp l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | h1 :: t1, h2 :: t2 ->
      if cmp h1 h2 = 0 then compare_list cmp t1 t2
      else cmp h1 h2
;;
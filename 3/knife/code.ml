let rec knife l i =
  if i <= 0 then ([], l)
  else match l with
    | [] -> ([], [])
    | h :: t -> let res = knife t (i - 1) in (h :: fst res, snd res)
;;
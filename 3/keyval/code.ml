let apply f k = List.fold_left (fun acc x ->
  match acc with
  | Some _ -> acc
  | None -> if fst x = k then Some (snd x) else None) 
  None f
;;
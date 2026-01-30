let rec extract i l = 
  if i < 0 || i >= List.length l then failwith "Out of bound"
  else match l with
  | [] -> failwith "Not possible"
  | h :: t -> if i = 0 then (h, t) else let (x, rest) = extract (i - 1) t in
                                        (x, h :: rest)
;; 
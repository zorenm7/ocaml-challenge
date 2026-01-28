let rec has_one n = 
  if n <= 0 then false
  else if n mod 10 = 1 then true else has_one (n/10)
;;
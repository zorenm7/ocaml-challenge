let rec sumrange a b = 
  if a > b then 0
  else a + sumrange (a+1) b
;;
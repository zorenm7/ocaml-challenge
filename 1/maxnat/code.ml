let max_nat a b = 
  if a > 0 && b > 0 then if a > b then a else b
  else failwith "One or both numbers are not natural"
;;
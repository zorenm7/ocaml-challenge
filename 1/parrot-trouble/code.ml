let parrot_trouble a x =
  if x < 0 || x > 23 then None
  else if a && (x >= 7 && x <= 20) then Some true else Some false 
;; 
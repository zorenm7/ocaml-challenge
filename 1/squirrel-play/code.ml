type season = Spring | Summer | Autumn | Winter;;

let squirrel_play x s = 
  if s = Summer then x >= 15 && x <= 35
  else x >= 15 && x <= 30
;; 
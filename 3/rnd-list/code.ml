let rec rnd_list n b = 
  if n <= 0 then []
  else (Random.int(b) + 1) :: rnd_list (n-1) b
;;

let rec rnd_list n b = match n with
| 0 -> []
| _ -> [Random.int(b + 1)] @ rnd_list (n - 1) b
;;
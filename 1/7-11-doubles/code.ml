let seven_eleven_doubles = 
  let random1 = Random.int(6) + 1 in 
  let random2 = Random.int(6) + 1 in 
  if ((random1 + random2) = 7 || (random1 + random2) = 11) || random1 = random2 then (true, random1, random2) else (false, random1, random2)
;;
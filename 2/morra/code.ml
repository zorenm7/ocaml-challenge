type winner = Player | Computer | Tie ;;

let win (hp,gp) = let hc = Random.int(5) in let gc = Random.int(10) in let computer = (hc, gc) in
  if (gp = (hc + hp)) && (gc != (hc + hp)) then (computer, Player) 
  else if ((gp != (hc + hp)) &&  (gc = (hc + hp))) then (computer, Computer)
  else (computer, Tie);; 
type grade = Val of int | CumLaude;;

let is_valid g = 
  match g with
  | Val n -> n >= 18 && n <= 30
  | CumLaude -> true
;;

let int_of_grade g = 
  match g with
  | Val n -> if is_valid g then n else failwith "Not valid"
  | CumLaude -> 32
;;

let avg l = let sum = List.fold_left (fun acc x -> acc + int_of_grade x) 0 l in
    float_of_int sum /. float_of_int (List.length l)
;;

let avg_norec l = l |> List.filter is_valid |> avg;;
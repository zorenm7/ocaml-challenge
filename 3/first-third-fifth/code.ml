let first_third_fifth l = 
  match l with 
  | a :: _ :: c :: _ :: e :: t when List.length l >= 5 -> Some (a, c, e) 
  | _ -> None
;;

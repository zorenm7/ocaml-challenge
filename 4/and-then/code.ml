let ( -?-> ) (o : 'a option) (next : 'a -> 'b option) : 'b option =
  match o with
  | None -> None
  | Some x -> next x
;;

let first_third_fifth l = 
  List.nth_opt l 0 -?-> fun x ->
  List.nth_opt l 2 -?-> fun y ->
  List.nth_opt l 4 -?-> fun z ->
    Some (x, y, z)
;;
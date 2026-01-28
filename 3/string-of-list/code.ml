let rec str l = 
  match l with
  | [] -> ""
  | [h] -> string_of_int h
  | h :: t -> string_of_int h ^ ";" ^ str t
;;

let string_of_list l = "[" ^ str l ^ "]";;
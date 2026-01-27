let option_map f x = match x with
| None -> None
| Some x -> Some (f x);;

let (<*>) f x = match f with
| None -> None
| Some f -> option_map f x;;

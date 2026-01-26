let max_offer a b = if a > b then a else b;;

let to_int a = match a with
| Some a -> a
| None -> 0;;

let best_offer a b c = match (a, b, c) with
| (None, None, None) -> None
| _ -> Some (max_offer(max_offer(to_int a) (to_int b)) (to_int c));;

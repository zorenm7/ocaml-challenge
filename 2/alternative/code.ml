let (<|>) x y = match x, y with
| None, None -> None
| None, _ -> y
| _, None -> x
| _ -> x
;;
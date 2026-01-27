let option_map f x = match x with
| None -> None
| Some x -> Some (f x);;
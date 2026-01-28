type 'a option = Some of 'a | None

let rec minmaxfun f a b =
  if a > b then None
  else if a = b then Some (f a, f a)
  else
    match minmaxfun f (a + 1) b with
    | None -> None
    | Some (min_rest, max_rest) -> Some (min (f a) min_rest, max (f a) max_rest)
;;
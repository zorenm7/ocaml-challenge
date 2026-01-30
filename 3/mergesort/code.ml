let rec merge l m = match l, m with
| h :: t, [] -> l
| [], h :: t -> m
| [], [] -> []
| h1 :: t1, h2 :: t2 -> if h1 < h2 then h1 :: merge t1 m else h2 :: merge l t2;;

let rec knife l i =
  if i <= 0 then ([], l)
  else match l with
    | [] -> ([], [])
    | h :: t -> let res = knife t (i - 1) in (h :: fst res, snd res)
;;

let rec halve l = knife l ((List.length l)/2);;

let rec merge_sort l = 
  match halve l with
  | ([], []) -> []
  | ([], _) -> l
  | (_, []) -> l
  | (sl, sl') -> merge (merge_sort sl) (merge_sort sl')
;;
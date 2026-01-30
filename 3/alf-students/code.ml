type student = {
  id: string;
  name: string;
  surname: string;
  vote: int option;
  laude: bool
}

let rec id_of_noshow l = 
  match l with
  | [] -> []
  | h :: t -> if h.vote = None then h.id :: (id_of_noshow t) else id_of_noshow t
;;

let rec upgradeable l =
  match l with
  | [] -> []
  | h :: t ->
      match h.vote with
      | Some v when v >= 15 && v <= 17 -> (h.name ^ " " ^ h.surname) :: upgradeable t
      | _ -> upgradeable t
;;

let rec upgrade l = 
  match l with
  | [] -> []
  | h :: t -> let h' =
        match h.vote with
        | Some v when v >= 15 && v <= 17 -> { h with vote = Some 18 }
        | _ -> h
      in
      h' :: upgrade t
;;

let rec wrong_laude l =
  match l with
  | [] -> []
  | h :: t ->
      (match h.vote with
       | Some v when v >= 30 -> wrong_laude t   
       | _ when h.laude -> (h.name ^ " " ^ h.surname) :: wrong_laude t
       | _ -> wrong_laude t)
;;

let rec fix_laude l =
  match l with
  | [] -> []
  | h :: t ->
      let h' =
        match h.vote with
        | Some 30 -> h                
        | _ -> {h with laude = false}   
      in
      h' :: fix_laude t
;;

let percent_passed l =
  let rec count_passed l =
    match l with
    | [] -> 0
    | h :: t ->
        match h.vote with
         | Some v when v >= 18 -> 1 + count_passed t
         | _ -> count_passed t
  in
  if List.length l = 0 then 0
  else (count_passed l * 100) / List.length l
;;

let avg_vote l =
  let passed = List.filter (fun s ->
    match s.vote with
    | Some v when v >= 18 -> true
    | _ -> false
  ) l in
  let sum = List.fold_left (fun acc s ->
    match s.vote with
    | Some v -> acc + (if s.laude then v + 2 else v)
    | None -> acc
  ) 0 passed in
  if List.length passed = 0 then 0.0
  else (float_of_int sum) /. (float_of_int (List.length passed))
;;
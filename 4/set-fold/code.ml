let mem x l = List.fold_left (fun acc y -> acc || y = x) false l;;

let mem x l = List.filter (fun y -> y = x) l |> List.length <> 0;;

let subseteq xl yl = List.fold_left (fun acc y -> acc && mem y yl) true xl;;

let subseteq xl yl = (List.filter (fun x -> mem x yl) xl) = xl;;

let seteq xl yl =
  List.fold_left (fun acc x -> acc && mem x yl) true xl
  &&
  List.fold_left (fun acc y -> acc && mem y xl) true yl
;;

let dup l =
  List.fold_left (fun acc x -> acc || (List.length (List.filter (fun y -> y = x) l) > 1)) false l
;;

let count x l = List.filter(fun y -> x = y) l |> List.length;;
let dup l = List.filter (fun x -> count x l = 1) l |> List.length <> List.length l;;

let mkset l = List.fold_left(fun acc x -> if mem x acc then acc else acc @ [x]) [] l;;

let union a b = mkset (a @ b);;

let inter a b = List.filter(fun y -> mem y a) b;;

let diff a b = List.filter(fun y -> not (mem y b)) a;;

let dsum xl yl = List.map (fun x -> (0, x)) xl @ List.map (fun y -> (1, y)) yl

let powset l = List.fold_left (fun acc x -> acc @ List.map (fun y -> y @ [x]) acc) [[]] l;;


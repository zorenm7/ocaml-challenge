type suit = Spades | Hearts | Diamonds | Clubs;;
type card = Card of int * suit;;

let checkCard c =
  match c with
  | Card (n, _) -> n >= 1 && n <= 10
;;

let unique l =
  let rec aux seen = function
    | [] -> true
    | h :: t -> if List.mem h seen then false else aux (h :: seen) t
  in aux [] l
;;

let is_complete l =
  List.length l = 40 && List.for_all checkCard l && unique l
;;

let suits = [Spades; Hearts; Diamonds; Clubs];;

let suits = [Spades; Hearts; Diamonds; Clubs];;

let gen_deck () =
  List.flatten (
    List.map (fun s ->
      List.init 10 (fun n -> Card (n+1, s))
    ) suits
  )
;;
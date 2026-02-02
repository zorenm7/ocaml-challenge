type ('a,'b) fsa = {
  trans: ('a * 'b * 'a) list;      (* set of transitions *)
  init: 'a;                        (* initial state *)
  final: 'a list                   (* final states *)
}

let rec subseteq xl yl = match xl with
    [] -> true
  | x::xl' -> List.mem x yl && subseteq xl' yl;;

let seteq xl yl = (subseteq xl yl) && (subseteq yl xl);;

(* m1 deterministic and complete *)
let m1 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2);(2,'1',2)];
  init = 0;
  final = [1] }
;;

(* m2 non-deterministic and non-complete *)
let m2 = { 
  trans = [(0,'0',0);(0,'0',1);
           (1,'0',2);(1,'1',2);
           (2,'0',2)];
  init = 0;
  final = [1] }
;;

(* m3 deterministic and non-complete *)
let m3 = { 
  trans = [(0,'0',0);(0,'1',1);
           (1,'0',1);(1,'1',2)];
  init = 0;
  final = [1;2] }
;;

let getlabels m =
  let rec aux l =
    match l with
    | [] -> []
    | (_, s, _) :: t -> s :: aux t
  in
  aux m.trans
;;

let outlabels m q =
  let rec aux l =
    match l with
    | [] -> []
    | (q', s, _) :: t -> if q' = q then s :: aux t else aux t
  in
  aux m.trans
;;

let rec mem x l =
  match l with
  | [] -> false
  | h :: t -> h = x || mem x t
;;

let rec mkset l =
  match l with
  | [] -> []
  | h :: t -> if mem h t then mkset t else h :: mkset t
;;

let getstates m =
  let rec aux l =
    match l with
    | [] -> []
    | (s, _, f) :: t -> s :: f :: aux t
  in
  mkset (aux m.trans)
;;

let rec support_iscomplete states labels m = match states with
| [] -> true
| h :: t -> (seteq (outlabels m h) (labels)) && support_iscomplete t labels m;;

let is_complete m = support_iscomplete (getstates m) (getlabels m) m;; 


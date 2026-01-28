let rec rev l = 
  match l with
  | [] -> []
  | [h] -> [h]
  | h :: t -> rev t @ [h]
;;

let is_palindrome l = l = rev l ;;

assert(is_palindrome []);;
assert(is_palindrome ['a';'n';'n';'a']);;
assert(is_palindrome ['r';'a';'d';'a';'r']);;
assert(is_palindrome ['a';'n';'n';'e'] = false);;
assert(is_palindrome ['z';'a';'n';'n';'a'] = false);;
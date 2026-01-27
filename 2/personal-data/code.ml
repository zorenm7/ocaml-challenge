type person = Person of (string * int * string option);;

let make_person n a e = if a > 150 || a < 0 then failwith "Out of range" else Person(n, a, e);;

let get_email (Person (n, a, e)) = e;; 

let can_send_adult_email (Person (n, a, e)) = if a >= 18 && e <> None then true else false;;
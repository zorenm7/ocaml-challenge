type person = {
  name : string;
  age : int;
  email : string option;
}

let make_person n a e = if a > 150 || a < 0 then failwith "Out of range" else { name = n; age = a; email = e };;

let get_email p = p.email;; 

let can_send_adult_email p = p.age >= 18 && p.email <> None;;
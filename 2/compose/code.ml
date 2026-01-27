let comp f g x = f (g x);;

let comp = fun f -> fun g -> fun x -> f (g x);;

let comp f g = fun x -> f (g x);;
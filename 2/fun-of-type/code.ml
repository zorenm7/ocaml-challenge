let f1 x = x > 0;;

let f2 x = if x then 1 else 0;;

let f3 x = (x, x >= 0);;

let f4 (x, y) = if y then x + 1 else x;;

let f5 x = fun f -> x+f;;

let f6 x = fun f -> x + f > 0;;

let f7 x = fun f -> if x || f >= 0 then true else false;;

let f8 x = fun f -> if x || f then 1 else 0;;

let f9 x = fun f -> if x || f >= 0 then 1 else 0;;

let f10 f = (f 0) + 1;;

let f11 f = if (f 0) then 1 else 0;;

let f12 f = (f true) + 1;;

let f13 f = if (f 0) then true else false;;

let f14 f = if (f true) then 1 else 0;;

let f15 x (y, z) = x + y + z;;

let f16 x = fun f -> fun g -> x + f + g;;

let f17 f = if f 0 >= 0 then f else f;;

let f18 f = f (fun x -> x + 1) + 1;;

let f19 f = fun g -> if f 0 > 0 then g true;;

let f19 f = fun g -> if f 0 > 0 then g else not g;;

let f20 f = fun g -> if f 0 then if g then 1 else 0 else if g then 1 else 0;;
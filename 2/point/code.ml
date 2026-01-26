type point = { x : float; y : float};;
let translate p x y = { x = p.x +. x; y = p.y +. y };;
let distance_from_origin p = Float.sqrt (p.x *. p.x +. p.y *. p.y);;

type color = Red | Green | Blue;;

type colored_point = { p : point; c : color};;

let same_color cp1 cp2 = cp1.c = cp2.c;;

let recolor cp newc = { p = cp.p; c = newc};;

(* TEST *)

let p1 = { x = 3.0; y = 4.0 };;
let p2 = translate p1 1.0 (-2.0);;

assert (p2.x = 4.0 && p2.y = 2.0);;
assert (distance_from_origin p1 = 5.0);;

let cp1 = { p = p1; c = Red };;
let cp2 = { p = p2; c = Red };;
let cp3 = { p = p2; c = Blue };;

assert (same_color cp1 cp2);;
assert (not (same_color cp1 cp3));;

let cp4 = recolor cp3 Green;;

assert (same_color cp4 { p = p1; c = Green });;
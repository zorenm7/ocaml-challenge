let mux2 s0 a b = 
  s0 && a || not s0 && b
;;

let mux2 s0 a b = 
  if s0 then a else b
;;

let mux2 s0 a b = 
  match s0 with
  | true -> a
  | false -> b
;;

let mux4 s1 s0 a0 a1 a2 a3 =
  let i1 = mux2 s0 a0 a1 in
  let i2 = mux2 s0 a2 a3 in
  mux2 s1 i1 i2
;;
let min2 a b = if a > b then b else a;;
let max2 a b = if a > b then a else b;;

let minmax3 a b c = (min2 (min2 a b) c, max2 (max2 a b) c);;
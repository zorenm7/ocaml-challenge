type weekday = Mo | Tu | We | Th | Fr

type course = ALF | LIP

let isLecture d c = 
  match c, d with
  | ALF, Tu -> true
  | ALF, Th -> true
  | ALF, Fr -> true
  | LIP, We -> true
  | LIP, Th -> true
  | _ -> false
;;
type vote = StrongReject | WeakReject | WeakAccept | StrongAccept

let decide_exam v1 v2 v3 = 
  let is_accept v = 
    match v with
    | WeakAccept | StrongAccept -> true
    | WeakReject | StrongReject -> false
  in
  let count_accept =
    (if is_accept v1 then 1 else 0) +
    (if is_accept v2 then 1 else 0) +
    (if is_accept v3 then 1 else 0)
  in
  let has_strong_reject =
    match v1, v2, v3 with
    | StrongReject, _, _ | _, StrongReject, _ | _, _, StrongReject -> true
    | _ -> false
  in
  count_accept >= 2 && not has_strong_reject
;;
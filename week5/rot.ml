let rotate a =
  try
    let n = Array.length a in
    let v = a.(0) in
    for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done;
    a.(n-1)<-v
  with
  | _ -> ();;


let rotate_by a n =
  let len = Array.length a in
  let k =
    if  n > 0 then n
    else
      len  + n in
  for i = 0 to (k-1) do
    rotate a
  done;;

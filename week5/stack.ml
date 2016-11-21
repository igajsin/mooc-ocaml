type stack = int array
exception Full
exception Empty;;

let create size = Array.make (size + 1) 0 ;;

let push buf elt =
  let pos = buf.(0) + 1 in
  try
    buf.(pos) <- elt;
    buf.(0) <- pos
  with
  | _ -> raise Full;;

let append buf arr =
  let len = Array.length arr in
  try
    for i = len - 1 downto 0 do
      push buf arr.(i)
    done
  with
  | _ -> raise Full;;

let pop buf =
  let pos = buf.(0) in
  if pos < 1 then raise Empty
  else
    begin
      buf.(0) <- pos - 1;
      buf.(pos)
    end;;

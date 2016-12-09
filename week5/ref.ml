exception Empty ;;

let swap ra rb =
  let tmp = !rb in
  begin
    rb := !ra;
    ra := tmp
  end;;


let update r f =
  let old = !r in
  begin
    r := f old;
    old
  end;;

let move l1 l2 =
  let h1 = try List.hd !l1 with | _ -> raise Empty
  and t1 = List.tl !l1 in
  begin
    l1 := t1;
    l2 := h1 :: !l2
  end;;


let reverse l =
  let l2 = ref [] in
  let mv l1 l2 =
    try
      while true do
        move l1 l2
      done;
    with
    | _ -> () in
  begin
    mv (ref l) l2;
    !l2
  end;;

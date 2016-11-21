let is_multiple i x = i mod x = 0

let output_multiples x n m =
  for i=n to m do
    if is_multiple i x then
      begin
        print_int i;
        print_string ","
      end
  done;;

exception Zero;;

let display_sign_until_zero f m =
  try
    for i = 0 to m do
      let res = f i in
      if res > 0 then
        print_string "positive\n"
      else if res < 0 then
        print_string "negative\n"
      else
        raise Zero
    done
  with
  | _ -> print_string "zero\n";;

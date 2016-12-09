type prs = Buffer.t * string list;;
type ltable = (string * string list) list;;

let is_num_char c = 
  let idx = Char.code c in
  idx > 47 && idx < 58;;

let is_alphabet_char c =
  let idx = Char.code c in
  (idx > 47 && idx < 58)
  || (idx > 64 && idx < 91)
  || (idx > 96 && idx < 123);;

let is_non_alphabet_char c =
  not (is_alphabet_char c);;

let mv (buf, strl) =
  let new_buf = Buffer.create 0 in
  if buf = new_buf
  then (*buf is empty*)
    (buf, strl)
  else
    let new_str = Buffer.contents buf in
    (new_buf, new_str::strl) ;;

let str_fold_left f acc str =
  let max_n = String.length str in
  let rec fold f n acc =
    if n = max_n
    then
      mv acc
    else
      let c = String.get str n in
      fold f (n + 1) (f acc c) in
  let (_, res) = fold f 0 acc in
  res;;

let words words =
  let fn (buf, lst) c =
    if (is_non_alphabet_char c) then
      mv (buf, lst)
    else
      let (_) = Buffer.add_char buf c in
      (buf, lst) in
  let init_buf = Buffer.create 0 in
  let acc = (init_buf, []) in
  List.rev (str_fold_left fn acc words);;

let build_ltable wlst =
  let add_to acc w in
  
  let rec fill acc = function
    | [] -> acc
    | w::ws -> add_to acc w in

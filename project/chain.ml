type prs = Buffer.t * string list;;
type ltable = (string * string list) list;;

let words words =
  let mv (buf, strl) =
    let new_buf = Buffer.create 0 in
    if buf = new_buf
    then (*buf is empty*)
      (buf, strl)
    else
      let new_str = Buffer.contents buf in
      (new_buf, new_str::strl) in

  let fn (buf, lst) c =
    let is_non_alphabet_char c =
      let is_alphabet_char c =
        let idx = Char.code c in
        (idx > 47 && idx < 58)
        || (idx > 64 && idx < 91)
        || (idx > 96 && idx < 123) in
      not (is_alphabet_char c) in

    if (is_non_alphabet_char c) then
      mv (buf, lst)
    else
      let (_) = Buffer.add_char buf c in
      (buf, lst) in

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
    res in

  let init_buf = Buffer.create 0 in
  let acc = (init_buf, []) in
  List.rev (str_fold_left fn acc words);;

let test_ltable = [ ("z", [ "y" ]);
                    ("x", [ "y" ; "y" ]);
                    ("START", [ "x" ]);
                    ("y", [ "x" ; "z" ; "STOP" ]) ];;

let build_ltable wlst =
  let rec add_to ltbl key value =
    match ltbl with
    | [] -> [(key, [value])]
    | (k,v)::xs ->
       if k = key then
         (k, v @ [value])::xs
       else
         (k,v)::add_to xs key value in

  let (last, ltbl) =
    List.fold_left
      (fun (prev, ltbl) word ->
        let new_ltbl = add_to ltbl prev word in
        (word, new_ltbl))
      ("START", []) wlst in
  add_to ltbl last "STOP";;

let next_in_ltable ltbl word =
  let rec in_ltable ltable key =
    match ltable with
    | [] -> None
    | (k, v)::xs ->
       if k = key then Some v
       else in_ltable xs key in
  let successors = in_ltable ltbl word in
  match successors with
  | None -> ""
  | Some wlst -> List.nth wlst (Random.int (List.length wlst));;

let walk_ltable ltbl =
  let rec step prev =
    let word = next_in_ltable ltbl prev in
    if word = "STOP" then []
    else
      word :: (step word) in
  step "START";;

let rec display_quote strings =
  let str = List.fold_left
              (fun acc string -> acc ^ " " ^ string)
              "" strings in
  String.sub str 1 (String.length str - 1);;

(* Part B *)

type distribution =
  { total : int ;
    amounts : (string * int) list };;

type htable = (string, distribution) Hashtbl.t;;

let compute_distribution words =
  let (tot, am) = List.fold_left
    (fun (tot, am) word ->
      match am with
      | [] -> (tot + 1, [(word, 1)])
      | (w, count)::ws when w = word -> (tot + 1, (w, count + 1)::ws)
      | (w, count)::ws as wlist -> (tot + 1, (word, 1)::wlist))
    (0, []) (List.sort compare words) in
  {total = tot; amounts = am};;

let build_htable words =
  let ht = Hashtbl.create (Random.int (List.length words)) in
  let result = Hashtbl.create (Hashtbl.length ht) in
  let add_to key value =
    if Hashtbl.mem ht key then
      Hashtbl.replace ht key (value::(Hashtbl.find ht key))
    else
      Hashtbl.add ht key [value] in
  let last =
    List.fold_left
      (fun prev word ->
        begin
          add_to prev word;
          word
        end
      ) "START" words in
  begin
    add_to last "STOP";
    Hashtbl.iter
      (fun key value ->
        Hashtbl.add result key (compute_distribution value))
      ht;
    result
  end;;

let next_in_htable words word =
  let distribution = Hashtbl.find words word in
  let pos = Random.int distribution.total + 1 in
  let rec find_word pos = function
    | [] -> ""
    | (word, amount)::ws ->
       let new_pos = pos - amount in
       if new_pos > 0 then
         find_word new_pos ws
       else
         word in
  find_word pos distribution.amounts;;

let walk_htable words =
  let rec step prev =
    let word = next_in_htable words prev in
    if word = "STOP" then []
    else
      word :: (step word) in
  step "START";;

(* PART C*)

let is_word c =
  let idx = Char.code c in
  let is_ab idx =
    (idx > 47 && idx < 58)
    || (idx > 64 && idx < 91)
    || (idx > 96 && idx < 123)
    || (idx > 127) in
  is_ab idx;;

let is_punctuation c =
    List.mem c [';'; ','; ':'; '-'; '"'; '\''; '?'; '!' ; '.'] ;;
let is_sentence_separator c = List.mem c ['?'; '!'; '.'];;

let split_word str =
  let buf = Buffer.create 3 in
  let res = ref [] in
  let flash_buf ()=
    let word = Buffer.contents buf in
    if not (word = "") then
      (res := word :: !res;
       Buffer.clear buf) in
  let convert idx c =
    if is_word c then (*part of uninterrupted sequences*)
      Buffer.add_char buf c
    else if is_punctuation c then
      (flash_buf ();
       res := (String.make 1 c) :: !res)
    else (* is separator *)
      flash_buf () in
  begin
    String.iteri convert str;
    flash_buf ();
    List.rev !res
  end;;

let split_sentence words =
  let is_sentence_separator s =
    s = "!" || s = "?" || s = "." in
  let add2end lst = function
	   | [] -> lst
	   | x -> lst @ [x] in
  let (sentence, sentences) =
    List.fold_left
      (fun (sentence, sentences) word ->
        if is_sentence_separator word then
          ([], add2end sentences (sentence @ [word]))
      else
        (sentence @ [word], sentences)
      ) ([],[]) words in
  add2end sentences sentence;;

let sentences str =
  split_sentence (split_word str);;

type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t };;

let rec start = function
  | 0 -> []
  | n -> "START" :: start (n - 1);;

let shift words word =
  let base = match words with
    | [] -> []
    | x::xs -> xs in
  base @ [word];;

let add_to table k v =
  if Hashtbl.mem table k then
    let vs = Hashtbl.find table k in
    Hashtbl.replace table k (v::vs)
  else
    Hashtbl.add table k [v] ;;

let build_ptable words n =
  let table = Hashtbl.create 0 in
  let result = Hashtbl.create 0 in
  let last =
    List.fold_left
      (fun key x ->
        (add_to table key x;
        shift key x)) (start n) words in
  (add_to table last "STOP";
   Hashtbl.iter (fun k v -> Hashtbl.add result k (compute_distribution v)) table;
   {prefix_length = n; table = result});;

let walk_ptable ptable =
  let first = start ptable.prefix_length in
  let table = ptable.table in
  let rec step prevs =
    let word = next_in_htable table prevs in
    if word = "STOP" then []
    else
      word :: (step (shift prevs word)) in
  step first;;




exception Inconsistent_prefix of int * int;;

let merge_ptables ptables =
  let common_prefix prefixes =
    List.fold_left
      (fun prev_prefix x ->
        if prev_prefix = x then x
        else
          raise (Inconsistent_prefix (prev_prefix, x)))
      (List.hd prefixes)
      (List.tl prefixes) in
  let add table k dist =
    let add_distributions d1 d2 =
      let total = d1.total + d2.total in
      let rec put (k, v) = function
        | [] -> [(k, v)]
        | (k1, v1)::xs when k = k1 -> (k, v + v1) :: xs
        | (k1, v1)::xs -> (k1, v1)::(put (k, v) xs) in
      let amounts =
        List.fold_left (fun acc x -> put x acc) d1.amounts d2.amounts in
      {total = total; amounts = amounts} in
    if Hashtbl.mem table k then
      let d1 = Hashtbl.find table k in
      Hashtbl.replace table k (add_distributions d1 dist)
    else
      Hashtbl.add table k dist in
  let tables = List.map (fun x -> x.table) ptables in
  let prefixes = List.map (fun x -> x.prefix_length) ptables in
  let prefix = common_prefix prefixes in
  let result = List.hd tables in
  begin
    List.iter
      (fun tbl -> Hashtbl.iter (fun k v -> add result k v) tbl)
      (List.tl tables);
    {prefix_length = prefix; table = result}
  end;;

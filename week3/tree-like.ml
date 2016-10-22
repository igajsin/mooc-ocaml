type trie = Trie of int option * char_to_children
 and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let rec children_from_char m c =
  match m with
  | [] -> None
  | x::xs ->
     let (c1, t) = x in
     if c = c1 then Some t
     else children_from_char xs c;;

let rec update_children m c t =
  match m with
  | [] -> [(c,t)]
  | x::xs ->
     let (c1, t1) = x in
     if c=c1
     then
       (c,t)::xs
     else
       x::(update_children xs c t);;

let lookup trie w =
  let stop n = n = String.length w in
  let extract = function
    | Trie (x, _) -> x in
  let rec depper t n =
    if stop n then
      extract t
    else
      let cur = String.get w n in
      match t with
      | Trie (r, childs) ->
         match children_from_char childs cur with
         | Some x -> depper x (n + 1)
         | _ -> None in
  depper trie 0;;

let exclude lst elt =
    let p x =
      let (c, _) = x in
      c = elt in
    let rec filter_tree proc lst =
      match lst with
      | [] -> []
      | x::xs -> if proc x then
                   filter_tree proc xs
                 else
                   x::(filter_tree proc xs) in
    filter_tree p lst;;

let insert trie w v =
  let stop n = String.length w = n in
  let rec step t n =
    match t with
    | Trie (r, cs) ->
        if stop n then Trie (Some v, cs)
        else
          let cur = String.get w n in
          let c_branch = children_from_char cs cur and
              other_branch = exclude cs cur in
          let h =
            match c_branch with
            | None -> (cur, step empty (n + 1))
            | Some x -> (cur, step x (n + 1)) in
          Trie (r, h::other_branch) in
  step trie 0;;

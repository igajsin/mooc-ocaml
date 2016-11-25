(*CHAR INDEXED HASHTABLES*)

module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
  val print_int_trie : int trie -> unit
  val add1 : 'a trie -> char -> 'a -> unit
end;;

module CharHashedType : Hashtbl.HashedType with type t = char =
  struct
    type t = char
    let equal x y = x = y
    let hash x = Char.code x
  end;;

module CharHashtbl = Hashtbl.Make (CharHashedType);;

module Trie : GenericTrie
       with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table
  let empty () =
    Trie (None, CharHashtbl.create 0);;

  let string2char_list str =
    let l = ref [] in
    String.iter (fun c -> l := c :: !l) str;
    List.rev !l;;

(*
  let lookup trie w =
    match trie with
    | Trie (x, _) -> x;;*)

  let lookup trie w =
    let wlst = string2char_list w in
    let rec lookup trie wlst =
      match (trie, wlst) with
      | (Trie (x, _), []) -> x
      | (Trie (_, h), x::xs) -> lookup (CharHashtbl.find h x) xs in
    try
      lookup trie wlst
    with
    |_ -> None;;

  let add1 (Trie (_, h)) k v =
  let value = Trie (Some v, CharHashtbl.create 0) in
  CharHashtbl.add h k value;;

  let rec print_int_trie (Trie (v, h)) =
    let print_optional = function
      | None -> print_string "None "
      | Some x -> print_string ((string_of_int x) ^ " ") in
    let rec print_hsh h =
      if (CharHashtbl.length h = 0) then ()
      else
	CharHashtbl.iter
	  (fun k vt ->
	    begin
              print_char ' ';
	      print_char k;
	      print_string ": ";
              print_int_trie vt
	    end
	  )
	  h in
    begin
      print_char '(';
      print_optional v;
      print_hsh h;
      print_char ')';
    end;;

  let insert trie w v =
    let wlst = string2char_list w in
    let rec insrt trie str v =
      let mktrie v = Trie (Some v, CharHashtbl.create 0) in
      match str with
      | [] -> trie
      | x::[] -> let (Trie (_, h)) = trie in
                 begin
                   CharHashtbl.replace h x (mktrie v);
                   trie
                 end
      | x::xs ->
         let (Trie (_, h)) = trie in
         let trie1 = try CharHashtbl.find h x
                     with |_ ->
                            begin
                              CharHashtbl.add h x (empty ());
                              CharHashtbl.find h x
                            end in
         insrt trie1 xs v in
    let (_) = insrt trie wlst v in
    trie;;

end;;

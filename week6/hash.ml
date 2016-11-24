(*CHAR INDEXED HASHTABLES*)

module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
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
    lookup trie wlst;;

  let insert trie w v =
    let wlst = string2char_list w in
    let rec insert trie wlst =
      match (trie, wlst) with
      | (Trie (_, h), []) -> Trie (Some v, h)
      | (Trie (k, h), x::xs) -> Trie (k, h)
    in
    insert trie wlst;;


  let add1 trie k v =
    let Trie (_, h1) = trie
    and value = Trie (Some v, CharHashtbl.create 0) in
    CharHashtbl.replace h1 k value;;

  let print_trie (Trie (_, h)) =
    let rec range low high = if low = high then [] else low::(range (low + 1) high) in
    let all_char = List.map Char.chr (range 0 256) in
    let print_node = function
    | (x, Trie (y, _)) ->
       match y with
       | None -> (print_char x; print_string ": None\n")
       | Some z -> (print_char x; print_string ": "; print_int z; print_string "\n") in
    let keys = List.filter (fun c -> CharHashtbl.mem h c ) all_char in
    let pairs = List.map (fun k -> (k, CharHashtbl.find h k)) keys in
    List.iter print_node pairs;;

end;;

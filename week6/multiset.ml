(* MULTISET *)

module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end

module MultiSet = struct

  type 'a item = {value: 'a; amount: int}
   and 'a t =
     | Empty
     | List of 'a item * 'a t

  let rec occurrences s x =
    match s with
    | Empty -> 0
    | List (a, s') when a.value = x -> a.amount
    | List (_, s') -> occurrences s' x

  let empty = Empty

  let rec insert s x =
    match s with
    | Empty -> List ({value = x; amount = 1}, Empty)
    | List (a, s') when a.value = x -> List ({value = x; amount = a.amount + 1}, s')
    | List (a, s') -> List (a, insert s' x)

  let rec remove s x =
    match s with
    | Empty -> Empty
    | List (a, s') when a.value = x ->
       if a.amount > 0 then
         List ({value = a.value; amount = a.amount - 1}, s')
       else
         s'
    | List (a, s') -> List (a, remove s' x)

end ;;

let letters word =
  let s = MultiSet.empty in
  let n = (String.length word) - 1 in
  let rec fill_set s = function
    | 0 -> MultiSet.insert s (String.get word 0)
    | n when n > 0 ->
       fill_set (MultiSet.insert s (String.get word n)) (n - 1)
    | _ -> MultiSet.empty in
  fill_set s n;;

let anagram word1 word2 =
  let s1 = letters word1
  and s2 = letters word2 in
  let rec ok s = function
    | MultiSet.Empty -> true
    | MultiSet.List (a, s') -> a.amount = MultiSet.occurrences s2 a.value && ok s s' in
  (ok s1 s2) && (ok s2 s1);;

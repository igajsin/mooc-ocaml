type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))

let rec to_list = function
  | CEmpty -> []
  | CSingle x -> [x]
  | CApp (a, b) -> (to_list a) @ (to_list b);;

let cexample = to_list example;;

let rec of_list = function
  | [] -> CEmpty
  | x::xs ->
    CApp (CSingle x, of_list xs);;

let append acl bcl =
  match (acl, bcl) with
  | (CEmpty, _) -> bcl
  | (_, CEmpty) -> acl
  | _ -> CApp (acl, bcl);;

let rec hd = function
  | CEmpty -> None
  | CSingle x -> Some x
  | CApp (a, b) ->
    let res = hd a in
    if res = None then
      hd b
    else
      res;;

let rec tl = function
  | CEmpty -> None
  | CSingle x -> Some CEmpty
  | CApp (a, b) ->
    match a with
    | CEmpty -> if b != CEmpty then Some b
    | CSingle _ -> Some b
    | _ ->
      match (tl a) with
      | Some x -> Some (CApp (x, b))
      | None -> tl b;;

let rec tl = function
  | CEmpty -> None
  | CSingle x -> Some CEmpty
  | CApp (a, b) ->
    match (a, b) with
    | (CEmpty, _) -> tl b
    | (CSingle _, x) -> Some x
    | _ ->
      match tl a with
      | None -> tl b
      | Some x -> Some (CApp (x,b));;
    


type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list;;

let rec print_path = function
  | [] -> ()
  | h::tl when not (tl = [])-> (print_string (h^"/"); print_path tl)
  | h::_ -> print_string (h ^ "\n");;

let rec print_file lvl name =
  if lvl <= 0 then
    print_string name
  else
    (print_string "|"; print_file (lvl - 1) name);;

let rec print_symlink lvl name path =
  print_file lvl name;
  print_string " -> ";
  print_path path;;

let rec print_dir lvl name =
  if lvl <= 0 then
    (print_string "/"; print_string name; print_char '\n')
  else
    (print_string "|"; print_dir (lvl - 1) name);;

let print_filesystem root =
  let rec print_filesystem1 lvl items =
    match items with
    | [] -> ()
    | (name, File)::l ->
        (print_file lvl name; print_char '\n';
         print_filesystem1 lvl l)
    | (name, Symlink path)::l ->
        (print_symlink lvl name path;
         print_filesystem1 lvl l)
    | (name, Dir fs)::l ->
        (print_dir lvl name;
         print_filesystem1 (lvl + 1) fs;
         print_filesystem1 lvl l)
  in
  print_filesystem1 0 root ;;

let rec resolve sym path =
  let rec resolve acc path =
    match (acc, path) with
    | (_, []) -> acc
    | (x::xs, y::ys) ->
       if y = ".." then resolve xs ys
       else resolve (y::acc) ys
    | (_, y::ys) ->
       if y = ".." then resolve [] ys
       else resolve (y::acc) ys in

  List.rev (resolve (List.tl (List.rev sym)) path) ;;

let rec file_exists root path =
  let rec mem p = function
    | [] -> None
    | x::xs -> if p x then Some x
               else
                 mem p xs in
  match path with
  | [] -> true
  | x::xs ->
     match mem (fun (name, _ ) ->
                            x = name) root with
     | None -> false
     | Some (_, Dir ds) -> file_exists ds xs
     | Some (_, File) -> file_exists [] xs
     | Some (_, Symlink _) -> false;;

let print_filesystem root =
  let rec print_filesystem1 acc lvl items =
    match items with
    | [] -> ()
    | (name, File)::l ->
        (print_file lvl name; print_char '\n';
         print_filesystem1 acc lvl l)
    | (name, Symlink path)::l ->
       if file_exists root (resolve (acc@[name]) path) then
         (print_symlink lvl name path;
          print_filesystem1 acc lvl l)
       else
         (print_file lvl name; print_string " -> "; print_string "INVALID\n";
          print_filesystem1 acc lvl l)
    | (name, Dir fs)::l ->
        (print_dir lvl name;
         print_filesystem1 (List.rev (name::acc)) (lvl + 1) fs;
         print_filesystem1 acc lvl l)
  in
  print_filesystem1 [] 0 root ;;

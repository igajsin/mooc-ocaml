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

let root_dir = [ "photos", Dir
    [ "march", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "photo_3.bmp", File ;
          "index.html", File ] ;
      "april", Dir
        [ "photo_1.bmp", File ;
          "photo_2.bmp", File ;
          "index.html", File ] ] ;
  "videos", Dir
    [ "video1.avi", File ;
      "video2.avi", File ;
      "video3.avi", File ;
      "video4.avi", File ;
      "best.avi", Symlink [ "video4.avi" ] ;
      "index.html", File ] ;
  "indexes", Dir
    [ "videos.html",
      Symlink [ ".." ; "videos" ; "index.html" ] ;
      "photos_march.html",
      Symlink [ ".." ; "photos" ; "march" ; "index.html" ] ;
      "photos_april.html",
      Symlink [ ".." ; "photos" ; "april" ; "index.html" ] ;
      "photos_may.html",
      Symlink [ ".." ; "photos" ; "may" ; "index.html" ] ] ];;

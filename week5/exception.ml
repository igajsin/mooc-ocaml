type report = message list
and message = string * status
and status = Successful | Failed;;

type 'a result = Ok of 'a | Error of exn;;

let exec f x =
  try Ok (f x)
  with
    e -> Error e;;


let compare user reference to_string =
  match (user, reference) with
  | (Ok u, Ok r) when u = r -> ("got correct value " ^ to_string u, Successful)
  | (Ok u, Ok _) -> ("got unexpected value " ^ to_string u, Failed)
  | (Error ue, Error re) when ue = re -> ("got correct exception " ^ exn_to_string ue, Successful)
  | (Error ue, Error _) -> ("got unexpected exception " ^ exn_to_string ue, Failed)
  | (Ok u, Error e) -> ("got unexpected value " ^ to_string u, Failed)
  | (Error ue, Ok e ) -> ("got unexpected exception " ^ exn_to_string ue, Failed);;


let test user reference sample to_string =
  let rec tester n =
    if n = 0 then []
    else
      let x = sample () in
      let r = compare (exec user x) (exec reference x) to_string in
      r::tester (n - 1) in
  tester 10;;

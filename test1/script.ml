open Cmdliner;;

let rec fac = function
  | n when n <= 0 -> 0
  | 1 -> 1
  | n -> n * fac (n-1);;

let fac_arg =
  let doc = "Please give a positive number for factorial's calculation." in
  Arg.(value & opt int 1 & info ["f"; "factorial"] ~docv:"factorial" ~doc);;

let main n =
  let fq = fac n in
  Printf.printf "factorial of %n is %n\n" n fq;;

let main_t = Term.(const main $ fac_arg);;

let info =
  let doc = "General script for various calculation." in
  let man = [`S "Bugs"; `P "mail me to igor@gajsin.name";] in
  Term.info "script" ~version:"0.0.1" ~doc ~man;;

let () = match Term.eval (main_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0;;

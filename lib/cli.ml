let revolt () = print_endline "Revolt!"

let revolt_t = Cmdliner.Term.(const revolt $ const ())

let cmd =
  let open Cmdliner in
  Cmd.v (Cmd.info "revolt") revolt_t

let chorus count msg =
  for i = 1 to count
  do
    ignore i ; print_endline msg
  done

let count =
  let open Cmdliner in
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let main () =
  let open Cmdliner in
  exit (Cmd.eval cmd)



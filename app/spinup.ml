module Applicative = struct
  let map f x = 
    let open Cmdliner.Term in
    const f $ x

  let ( let+ ) x f = map f x

  let ( and+ ) ax ay =
    let open Cmdliner.Term in
    map (fun x y -> x, y) ax $ ay
end

module Applicative2 = struct
  let const x = Some x

  let ($) af ax =
    match af, ax with
    | Some f, Some x -> Some (f x)
    | _, _ -> None

  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let ( let+ ) x f = map f x

  let ( and+ ) ax ay =
    map (fun x y -> x, y) ax $ ay
end

let revolt () = print_endline "Revolt!"

let ax1 = Some 1

let ax2 = Some 2

let add = Some (+)

let revolt_t =
  let open Applicative in
  let open Cmdliner.Term in
  let+ r = const revolt
  in r ()

(* Cmdliner.Term.(const revolt $ const ()) *)

let handle_result handler = function
  | Ok actions ->
     handler actions
  | Error e -> begin
      let executable =
        Filename.basename Prelude.argv0
      in
      print_endline (executable ^ ": " ^ e) ;
      exit 1
    end

let doit handler name =
  let open Lib.Action in
  handle_result
    handler
    (main_action name)

let project_name name =
  let open Lib.Action in
  doit run name

let project_name_t dude =
  let open Cmdliner.Term in
  const project_name $ const dude

let chorus count msg =
  for i = 1 to count
  do
    ignore i ; print_endline msg
  done

let count =
  let open Cmdliner in
  let doc = "Repeat the message $(docv) times." in
  Arg.(value & opt int 10 & info ["c"; "count"] ~docv:"COUNT" ~doc)

let msg =
  let open Cmdliner in
  let env =
    let doc = "Overrides the default message to print." in
    Cmd.Env.info "CHORUS_MSG" ~doc
  in
  let doc = "The message to print." in
  Arg.(value & pos 0 string "Revolt!" & info [] ~env ~docv:"MSG" ~doc)

let chorus_t = Cmdliner.Term.(const chorus $ count $ msg)

let cmd =
  let open Cmdliner in
  let doc = "print a customizable message repeatedly" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <bugs@example.org>." ]
  in
  let info = Cmd.info "chorus" ~doc ~man in
  Cmd.v info chorus_t

let stuff () =
  let open Cmdliner in
  exit (Cmd.eval cmd)

module Main = struct
  let handle_result handler = function
    | Ok actions ->
       handler actions
    | Error e -> begin
        let executable =
          Filename.basename Prelude.argv0
        in
        print_endline (executable ^ ": " ^ e) ;
        exit 1
      end

  let main args =
    let open Lib.Action in
    let msg =
      "USAGE: spinup <project-name>"
    in
    let doit handler name =
      handle_result
        handler
        (main_action name)
    in
    match args with
    | ["--dry-run"; name] ->
       doit dry_run name
    | [name] ->
       doit run name
    | _ -> begin
        print_endline msg ;
        exit 1
      end
end
              
let () = stuff ()
  (* Main.main (Prelude.argv) *)

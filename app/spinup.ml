module Applicative = struct
  let map f x = 
    let open Cmdliner.Term in
    const f $ x

  let ( let+ ) x f = map f x

  let ( and+ ) ax ay =
    let open Cmdliner.Term in
    map (fun x y -> x, y) ax $ ay

  let pure = Cmdliner.Term.const
end

let revolt () = print_endline "Revolt!"

let revolt_t =
  let open Applicative in
  let open Cmdliner.Term in
  let+ r = const revolt
  in r ()

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

let chorus_t =
  let open Applicative in
  let open Cmdliner.Term in
  let+ chor = const chorus
  and+ c = count
  and+ m = msg
  in chor c m

let cmd =
  let open Cmdliner in
  let doc = "create an OCaml project skeleton" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <elucidations@uchicago.edu>." ]
  in
  let info = Cmd.info "spinup" ~doc ~man in
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

let doit handler name =
  let open Lib.Action in
  handle_result
    handler
    (main_action name)

(* let project_name name =
 *   let open Lib.Action in
 *   doit run name
 * 
 * let vflag = Cmdliner.Arg.(
 *     value
 *     @@ flag
 *     @@ info ["V";"version"]
 *          ~doc:"Display version information and metadata."
 *             )
 * 
 * let ntimes = Cmdliner.Arg.(
 *     value
 *     @@ opt int 1
 *     @@ info ["n";"ntimes"]
 *          ~docv:"N"
 *          ~doc:"Greet the user $$(docv) times."
 *              )
 * 
 * let project_name_t =
 *   let open Cmdliner.Term in
 *   let open Applicative in
 *   let+ pname =
 *     pure project_name
 *   in
 *   pname name *)

end

module type TERM = 
  Etude.Endofunctors_intf.Applicative.AUGMENTED

module Term : TERM
       with type 'a t = 'a Cmdliner.Term.t
  = struct
  include Cmdliner.Term

  module type BASIC =
    Etude.Endofunctors_intf.Applicative.BASIC

  module BasicApp : BASIC
         with type 'a t = 'a Cmdliner.Term.t
    = struct
    type 'a t = 'a Cmdliner.Term.t
    let map f x =
      const f $ x

    let product ax ay =
      map (fun x y -> x, y) ax $ ay
      
    let pure = const
  end

  open Etude.Endofunctors
  include Applicative.Make (BasicApp)

  let pure = const
end

module Arguments = struct
  let dry_run =
    let open Cmdliner.Arg in
    let doc = "Print description of what would happen\
               if $(tname) were run in normal mode."
    in
    let docv = "D" in
    let inf = info ["d"; "dry-run"] ~doc ~docv in
    let arg_type = flag in
    value (arg_type inf)

  let project_name =
    let open Cmdliner.Arg in
    let doc = "Name of project to be spun up." in
    let docv = "PROJECT_NAME" in
    let inf = info [] ~doc ~docv in
    let arg_type = pos ~rev:true 0 (some string) None in
    required (arg_type inf)

  let manpage =
    Cmdliner.
    [
      `S Manpage.s_description ;
      `P "stub description" ;
    ]
end

module Command = struct
  let main_term exe dry_run project_name =
    let open Term in
    let+ main = pure exe
    and+ dr = dry_run
    and+ pn = project_name
    in main dr pn |> ignore

  let manpage =
    Cmdliner.
    [
      `S Manpage.s_description ;
      `P "stub description" ;
    ]

  let manpage_info =
    let doc = "Spins up an OCaml project skeleton." in
    Cmdliner.Cmd.info "spinup" ~doc ~man:manpage

  let command exe dry_run project_name =
    let term = main_term exe dry_run project_name in
    Cmdliner.Cmd.v manpage_info term

  let to_exe exe dry_run project_name =
    let open Cmdliner.Cmd in
    exit (eval (command exe dry_run project_name))
end

let main () =
  assert false
              
let () = stuff ()
  (* Main.main (Prelude.argv) *)


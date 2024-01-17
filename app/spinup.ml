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

(* end *)


let main () =
  assert false
              
let () = ()
  (* Main.main (Prelude.argv) *)


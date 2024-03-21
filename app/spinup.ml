let handle_result handler =
  let open Lib.Global_error in
  function
  | Ok actions -> handler actions
  | Error e ->
    print e ;
    exit 1

let print_config () =
  let open Lib.Crunched_config in
  let msg =
    Lib.Filesystem.wrap_in_argv0 "internal crunch error"
  in
  match read ".spinuprc" with
  | Some conf -> print_endline conf
  | None ->
    print_endline msg ;
    exit 1

let main pc_arg dr_arg pname_arg =
  let open Lib.Action in
  let doit handler pname =
    handle_result handler (main_action'' pname)
  in
  match (pc_arg, dr_arg, pname_arg) with
  | true, _, _ -> print_config ()
  | _, true, Some pname -> doit dry_run pname
  | _, _, Some pname -> doit run pname
  | _, _, None ->
    let open Prelude.Message in
    message ~myself:"" ~exit:1
      "Usage: spinup [--dry-run] [--print-config] \
       [OPTION]...[PROJECT_NAME]\n\
       Try 'spinup --help' for more information."

let () =
  let open Lib.Cli in
  Command.to_exe main Arguments.print_config
    Arguments.dry_run Arguments.project_name

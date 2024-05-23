let handle_result handler =
  let open Lib.Global_error in
  function
  | Ok actions -> handler actions
  | Error e ->
    print e ;
    exit 1

let main pc_arg dr_arg pname_arg =
  let open Lib.Action.Main in
  let open Lib.Action.PrintConfig in
  let doit handler action = handle_result handler action in
  match (pc_arg, dr_arg, pname_arg) with
  | true, _, _ -> doit run print_config
  | _, true, Some pname -> doit dry_run (main_action pname)
  | _, _, Some pname -> doit run (main_action pname)
  | _, _, None ->
    let open Prelude.Message in
    message ~myself:"" ~exit:1
      "Usage: spinup [--dry-run] [--print-config] \
       [OPTION]...[PROJECT_NAME]\n\
       Try 'spinup --help' for more information."

let () =
  let open Lib.Cli.Arguments in
  let open Lib.Cli.Command in
  to_exe main print_config dry_run project_name

let handle_result handler =
  let open Lib.Error_message in
  function
  | Ok actions -> handler actions
  | Error e ->
    print e ;
    exit 1

let spinup_exe pc dr pname =
  let open Lib.Action.Main in
  let open Lib.Action.PrintConfig in
  let doit handler action = handle_result handler action in
  match (pc, dr, pname) with
  | true, _, _ -> doit run print_config
  | _, true, Some pname -> doit dry_run (action pname)
  | _, _, Some pname -> doit run (action pname)
  | _, _, None ->
    let open Prelude.Message in
    message ~myself:"" ~exit:1
      "Usage: spinup [--dry-run] [--print-config] \
       [OPTION]...[PROJECT_NAME]\n\
       Try 'spinup --help' for more information."

let () =
  let open Lib.Cli in
  run_cli ~spinup_exe

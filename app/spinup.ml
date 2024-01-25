let handle_result handler = function
  | Ok actions ->
     handler actions
  | Error e -> begin
      print_endline e ;
      exit 1 ;
    end

let print_crunch path =
  let open Lib.Crunched_config in
  let msg = Lib.Errors.wrap_in_argv0
              "internal crunch error"
  in
  match read path with
  | Some conf -> print_endline conf
  | None -> begin
      print_endline msg ;
      exit 1 ;
    end

let main dr_arg pname_arg =
  let open Lib.Action in
  let doit handler pname =
    handle_result
      handler
      (main_action pname)
  in
  match dr_arg, pname_arg with
  | true, pname -> doit dry_run pname
  | _, pname -> doit run pname
              
let () = let open Lib.Cli in
         Command.to_exe
           main
           Arguments.dry_run
           Arguments.project_name


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



module Main = struct
  let handle_result handler = function
    | Ok actions ->
       handler actions
    | Error e -> begin
        print_endline (Prelude.argv0 ^ ": " ^ e) ;
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
              
let () = Main.main (Prelude.argv)

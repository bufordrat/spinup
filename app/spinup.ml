module Main = struct
  let handle_result handler = function
    | Ok actions -> List.iter handler actions
    | Error e -> begin
        print_endline e ;
        exit 1
      end

  let main args =
    let open Lib.Action in
    match args with
    | ["--dry-run"; name] ->
       handle_result dry_run (main_actions name)
    | [name] ->
       handle_result run (main_actions name)
    | _ -> begin
        print_endline "USAGE: spinup <project-name>" ;
        exit 1
      end
end
              
let () = Main.main (Prelude.argv)

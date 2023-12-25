module Main = struct
  let handle_result handler = function
    | Ok actions ->
       List.iter handler actions
    | Error e -> begin
        print_endline e ;
        exit 1
      end
               
  let outer_dir handler name =
    let open Prelude in
    let open Lib.Action in
    let mkdir name =
      Opening.(run @@ mk_projectdir name)
    in
    let main name =
      handle_result
        handler
        (main_actions name)
    in
    mkdir name ;
    withcd (fun n -> main n) name

  let main args =
    let open Lib.Action in
    let msg =
      "USAGE: spinup <project-name>"
    in
    match args with
    | ["--dry-run"; name] ->
       outer_dir dry_run name
    | [name] ->
       outer_dir run name
    | _ -> begin
        print_endline msg ;
        exit 1
      end
end
              
let () = Main.main (Prelude.argv)

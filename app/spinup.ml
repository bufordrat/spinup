module Main = struct
  let main () =
    let open Lib.Action in
    let open Etude.Result.Make (String) in
    match main_actions "keith" with
    | Ok actions -> List.iter run actions
    | Error e -> begin
        print_endline e ;
        exit 1
      end
end
              
let () = Main.main ()

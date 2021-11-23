open Prelude

module Main = struct

  module Feather2IO = struct
    open Feather
    open Lib.Commands

    let mk_project_root name =
      mk_project name |> Feather.run
      
    let inside_the_dir name =
      foldl1 and_ [
          delete_bin
        ; init_executable name
        ; create_lib
        ; create_lib_dune
        ; create_exe name
        ; create_exe_dune name
        ; create_use_output
        ; create_ocamlinit
        ; create_dune_project name
        ] |> Feather.run

    let the_whole_thing name =
      mk_project_root name
      ; Unix.chdir name
      ; inside_the_dir name
      ; Unix.chdir ".."
  end
  open Feather2IO

  module Errors = struct
    let usage = "USAGE: spinup <project-name>"
    let warning =
      "Spinning up project anyway with default name \
       \"new_project\"..."

    let print_warning () =
      print usage
      ; print warning
  end
     
  let main () =
    let gimme_the_arg = function
      | [] -> None
      | x :: [] -> Some x
      | _ -> None
    in
    match gimme_the_arg argv with
    | None -> begin
        Errors.print_warning ()
      ; the_whole_thing "new_project"
      end
    | Some n -> the_whole_thing n
            
end
              
let () = Main.main ()

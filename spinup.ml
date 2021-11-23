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
        ] |> Feather.run ~cwd:name
  end
  open Feather2IO
            
  let main name =
    mk_project_root name
    ; inside_the_dir name

end
              
let () = Main.main "matt"

open Prelude

module Main = struct
  open Lib.Commands
  open Feather

  let prog name =
    foldl1 sequence 
      [ mk_project name
      ; cd name
      ; delete_bin
      ; init_executable name
      ; create_lib
      ; create_lib_dune
      ; create_exe name
      ; create_exe_dune name
      ; create_use_output
      ; create_ocamlinit
      ; create_dune_project name
      ]
end
              
let () = Feather.run @@ Main.prog "matt"

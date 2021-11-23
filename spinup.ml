open Prelude

module Main = struct
  open Lib.Commands.IO

  let prog name =
    prep_project name
    ; inside_the_dir name

end
              
let () = Feather.run @@ Main.prog "matt"

module Main = struct
  open Lib.Commands.IO

  let prog name =
    mk_project_root name
    ; inside_the_dir name

end
              
let () = Main.prog "matt"

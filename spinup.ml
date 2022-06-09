open Prelude

module type SETTINGS = Lib.Io.SETTINGS

module Io = Lib.Io
module Errors = Lib.Errors

module Main = struct

  module Verbosity : SETTINGS = struct
    let verbose = true
  end

  let main () =
    let open Io.BigPicture (Verbosity) in
    let open Errors in
    let open Mattlude.Endofunctors in
    let module R = Result.Make (E) in
    let open R in
    let project_name =
      gimme_the_arg argv
      >>= check_exists dir_or_file
    in
    match project_name with
    | Error e -> respond e
    | Ok n -> the_whole_thing n 

end
              
let () = Main.main ()

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
      ; withcd (fun _ -> inside_the_dir name) name
  end
  open Feather2IO

  module Errors = struct
    let usage = "USAGE: spinup <project-name>"
    let already_exists name file =
      String.concat " " [
          "Error: a" ;
          file ;
          "called" ;
          name ;
          "already exists." ;
        ]

    module E = struct
      type t =
        | BadArgv
        | AlreadyExists of (string * string)
    end
    include E
                         
    module Kleislis = struct
      open Feather
         
      let gimme_the_arg = function
        | [] -> Error BadArgv
        | x :: [] -> Ok x
        | _ -> Error BadArgv

      let filetype = function
          | "-f" -> "file"
          | "-d" -> "directory"
          | _ -> ""
             
      let check_exists option name =
        let test =
          process "test" [ option ; name ] |> collect status
        in
        match test with
        | 0 -> Error (AlreadyExists
                        (name,
                         (filetype option)))
        | _ -> Ok name

      let check_dir_exists name =
        check_exists "-d" name

      let check_file_exists name =
        check_exists "-f" name
    end
    include Kleislis
  end

  let main () =
    let open Errors in
    let open Mattlude.Endofunctors in
    let module R = Result.Make (E) in
    let open R in 
    let name =
      gimme_the_arg argv
      >>= check_dir_exists
      >>= check_file_exists
    in
    match name with
    | Error BadArgv -> begin
        Prelude.print usage
      end
    | Error (AlreadyExists (n, d)) -> begin
        Prelude.print @@ already_exists n d
      end
    | Ok n -> the_whole_thing n

end
              
let () = Main.main ()

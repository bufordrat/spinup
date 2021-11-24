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

    let dir_or_file name = 
      if Sys.is_directory name
      then "directory"
      else "file"
      
    module E = struct
      type t =
        | BadArgv
        | AlreadyExists of (string * string)
    end
    include E
                         
    module Kleislis = struct
         
      let gimme_the_arg = function
        | [] -> Error BadArgv
        | x :: [] -> Ok x
        | _ -> Error BadArgv
             
      let check_exists f name =
        if Sys.file_exists name
        then Error (AlreadyExists
                        (name,
                         (f name)))
        else Ok name
    end
    include Kleislis
  end

  let main () =
    let open Errors in
    let open Mattlude.Endofunctors in
    let module R = Result.Make (E) in
    let open R in
    let print = Prelude.print in
    let project_name =
      gimme_the_arg argv
      >>= check_exists dir_or_file
    in
    match project_name with
    | Error BadArgv ->
       print usage ;
       exit 1
    | Error (AlreadyExists (n, d)) ->
       print @@ already_exists n d ;
       exit 1
    | Ok n ->
       the_whole_thing n ;
       exit 0

end
              
let () = Main.main ()

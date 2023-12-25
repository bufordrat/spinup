open Prelude

module E = struct
  type dependency =
    | Dune
    | Opam
    
  type t =
    | BadArgv
    | AlreadyExists of (string * string)
    | MissingDependency of dependency

  let dependency_to_string = function
    | Dune -> "dune"
    | Opam -> "opam"
end
include E

module Messages = struct
  let usage = "USAGE: spinup <project-name>"
            
  let already_exists name dir_or_file =
    String.concat " " [
        "Error: a" ;
        dir_or_file ;
        "called" ;
        name ;
        "already exists." ;
      ]

  let dependencies =
    let deps = [Dune ; Opam ; ] in
    let dep_strings = map dependency_to_string deps in
    String.concat "\n" @@ [
        "spinup has the following dependencies:" ;
        "" ] @ dep_strings
    
  let missing_dep_message = function
    | Dune -> assert false
    | Opam -> assert false
end
include Messages      

module Responses = struct                
  let respond = function
    | BadArgv ->
       print usage ;
       exit 1
    | AlreadyExists (n, d) ->
       print @@ already_exists n d ;
       exit 1
    | MissingDependency dep ->
       print (missing_dep_message dep) ;
       print dependencies ;
       exit 1
end
include Responses
      
module Kleislis = struct

  (* TODO: don't let the user have a project name with a hyphen *)
  (* TODO: check for dependencies *)
 
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
    
  let dir_or_file name = 
    if Sys.is_directory name
    then "directory"
    else "file"
end
include Kleislis

(* open Prelude *)

module E = struct
  (* type dependency =
   *   | Dune
   *   | Opam
   *   
   * type t =
   *   | BadArgv
   *   | AlreadyExists of (string * string)
   *   | MissingDependency of dependency
   *   | MissingTemplate of string
   * 
   * let dependency_to_string = function
   *   | Dune -> "dune"
   *   | Opam -> "opam" *)
end
include E

module Messages = struct
  let usage = "USAGE: spinup <project-name>"

  (* let dependencies =
   *   let deps = [Dune ; Opam ; ] in
   *   let dep_strings = map dependency_to_string deps in
   *   String.concat "\n" @@ [
   *       "spinup has the following dependencies:" ;
   *       "" ] @ dep_strings *)
    
  (* let missing_dep_message = function
   *   | Dune -> assert false
   *   | Opam -> assert false *)
end
include Messages      
      
module Kleislis = struct

  (* TODO: don't let the user have a project name with a hyphen *)
  (* TODO: check for dependencies *)

  (* let gimme_the_arg = function
   *   | [] -> Error BadArgv
   *   | x :: [] -> Ok x
   *   | _ -> Error BadArgv *)

  (* TODO: check for missing templates *)

  let dir_or_file path = 
    if Sys.is_directory path
    then ("/", "directory")
    else ("", "file")
    
  let check_exists name =
    let open Prelude in
    let (slash, dir_or_file) =
      dir_or_file name
    in
    let msg = String.join ~sep:"" [
      "A " ;
      dir_or_file ;
      " called " ;
      name ; 
      slash ;
      " already exists." ; ]
    in
    if Sys.file_exists name
    then Error msg
    else Ok ()
end
include Kleislis


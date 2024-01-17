(* TODO: don't let the user have a project name with a hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

let dir_or_file path = 
  if Sys.is_directory path
  then ("/", "directory")
  else ("", "file")
  
let already_exists name =
  let open Prelude in
  let msg (slash, dir_or_file) =
    String.join ~sep:"" [
                  "a " ;
                  dir_or_file ;
                  " called " ;
                  name ; 
                  slash ;
                  " already exists." ; ]
  in
  if Sys.file_exists name
  then Error (msg @@ dir_or_file name)
  else Ok ()

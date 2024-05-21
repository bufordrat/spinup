(* TODO: don't let the user have a project name with a
   hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

module E = Filesystem_error
module Trace = Global_error.T

let dir_or_file path =
  if Sys.is_directory path
  then ("/", "directory")
  else ("", "file")

let already_exists name =
  let open Prelude in
  let open E.Smart in
  let prose (slash, dir_or_file) =
    String.join ~sep:""
      [ "a ";
        dir_or_file;
        " called ";
        name;
        slash;
        " already exists."
      ]
  in
  if Sys.file_exists name
  then
    let msg = dir_or_file name |> prose in
    Trace.new_error (dir_already_exists msg)
  else Ok ()

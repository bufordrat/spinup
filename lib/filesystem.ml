(* TODO: don't let the user have a project name with a
   hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

module E = Filesystem_error
module Trace = Global_error.T

let dir_or_file path =
  if Sys.is_directory path then E.Dir else E.File

let already_exists name =
  let open E.Smart in
  let current = Sys.getcwd () in
  let dof = dir_or_file name in
  if Sys.file_exists name
  then Trace.new_error (already_exists current dof name)
  else Ok ()

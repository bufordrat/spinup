(* TODO: don't let the user have a project name with a
   hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

module E = Filesystem_error
module Trace = Global_error.T

let dir_or_file path =
  match Sys.is_directory path with
  | exception Sys_error _ -> None
  | exception e -> raise e
  | true -> Some E.Dir
  | false -> Some E.File

let already_exists name =
  let open E.Smart in
  let current = Sys.getcwd () in
  match dir_or_file name with
  | Some dof ->
    Trace.new_error (already_exists current dof name)
  | None -> Ok ()

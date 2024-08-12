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

let validate_project_name pname =
  let open Prelude.Char in
  let open Prelude.String in
  let open Filesystem_error.Smart in
  let works_for_dune char =
    Alphabetic.is char || Decimal.is char || char = '_'
  in
  if all works_for_dune pname
  then Ok pname
  else Trace.new_error (bad_project_name pname)

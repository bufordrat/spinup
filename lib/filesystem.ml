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
  then Ok ()
  else Trace.new_error (bad_project_name pname)

module Prereqs = struct
  open Prelude.Prereq
  let exists x = Exists x
  let check_prereqs prereqs =
    let pprereqs = 
      check (List.map exists prereqs)
    in
    if succeeds pprereqs
    then Ok ()
    else
      let open Trace in
      let open E.Smart in
      let open Etude.Result.Make (String) in
      let binaries = errors pprereqs in
      new_error (missing_prereqs binaries)
end

let prereqs = [ "dune" ; "opam" ]
let check_prereqs = Prereqs.check_prereqs

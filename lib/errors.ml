(* TODO: don't let the user have a project name with a hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

let dir_or_file path =
  if Sys.is_directory path then ("/", "directory") else ("", "file")

let wrap_in_argv0 msg =
  let executable = Filename.basename Prelude.argv0 in
  executable ^ ": " ^ msg

let already_exists name =
  let open Prelude in
  let prose (slash, dir_or_file) =
    String.join ~sep:""
      [ "a "; dir_or_file; " called "; name; slash; " already exists." ]
  in
  if Sys.file_exists name
  then
    let msg = dir_or_file name |> prose |> wrap_in_argv0 in
    Error msg
  else Ok ()

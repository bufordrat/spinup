(* TODO: don't let the user have a project name with a hyphen *)
(* TODO: check for dependencies *)
(* TODO: check for missing templates *)

let check_template_exists t =
  let open Template.Unprocessed in
  let fullpath =
    t.template_path ^ "/" ^ t.template_filename
  in
  let msg =
    "template at " ^ fullpath ^ " is missing."
  in
  if Sys.file_exists fullpath
  then Ok ()
  else Error msg

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

let process unv =
  let open Template in
  let open Unprocessed in
  let open Etude.Result.Make (String) in
  let* () = check_template_exists unv in
  let context = unv.context in
  let write_path =
    unv.output_path ^ "/" ^ unv.output_filename in
  let vmessage = unv.umessage in
  let+ data = Engine.process_template
                ~template:unv.template_filename
                ~context:context
  in Processed.{ write_path ; data ; vmessage }

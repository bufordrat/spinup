open Prelude

module Constants = struct
  let use_output = {|(*
* provide 4.11's ocaml toplevel #use_output directive for pre-4.11 ocamls
* see: <https://dune.readthedocs.io/en/stable/toplevel-integration.html>
*)

#directory "+compiler-libs"

let try_finally ~always f =
  match f () with
  | x ->
    always ();
    x
  | exception e ->
    always ();
    raise e

let use_output command =
  let fn = Filename.temp_file "ocaml" "_toploop.ml" in
  try_finally
    ~always:(fun () -> try Sys.remove fn with Sys_error _ -> ())
    (fun () ->
      match
        Printf.ksprintf Sys.command "%s > %s" command (Filename.quote fn)
      with
      | 0 -> ignore (Toploop.use_file Format.std_formatter fn : bool)
      | n -> Format.printf "Command exited with code %d.@." n)

let () =
  let name = "use_output" in
  if not (Hashtbl.mem Toploop.directive_table name) then
    Hashtbl.add Toploop.directive_table name
      (Toploop.Directive_string use_output)

;;
#remove_directory "+compiler-libs" ;;
|}

  let ocamlinit = {|#use "./use-output.top" ;;
#use_output "opam exec -- dune top"  ;;
open Prelude ;;
#use "topfind";;
|}

  let dune_project project_name = sprintf {|(lang dune 2.7)
(name %s)
(generate_opam_files true)
|} project_name

  let lib =
    "let message = \"GOODBYE CRUEL WORLD (is underrated)\""

  let lib_dune = {|(library
 (name lib)
 (libraries prelude))
|}

  let exe =
    "let () = print_endline Lib.message"
    
  let exe_dune project_name = sprintf {|(executable
 (public_name %s)
 (name spinup %s)
 (libraries feather prelude lib))
|} project_name project_name
      
end

module Commands = struct
  open Feather
  open Infix
  open Constants
     
  let mk_project name =
    process "dune" [ "init" ; "project" ; name ]

  let cd name =
    process "cd" [ name ]

  let delete_bin =
    process "rm" [ "-R" ; "bin" ]

  let init_executable name =
    process "dune" [ "init" ; "exe" ; name ]

  let create_lib =
    echo lib > "lib/lib.ml"

  let create_lib_dune =
    echo lib_dune > "lib/dune"

  let create_exe name =
    echo exe > (name ^ ".ml")

  let create_exe_dune name =
    echo (exe_dune name) > "dune"

  let create_use_output =
    echo use_output > "use-output.top"

  let create_ocamlinit =
    echo ocamlinit > ".ocamlinit"

  let create_dune_project name =
    echo (dune_project name) > "dune-project"

  module IO = struct
    
    let mk_project_root name =
      mk_project name |> Feather.run

    let inside_the_dir name =
      foldl1 and_ [
          delete_bin
        ; init_executable name
        ; create_lib
        ; create_lib_dune
        ; create_exe name
        ; create_exe_dune name
        ; create_use_output
        ; create_ocamlinit
        ; create_dune_project name
        ] |> Feather.run ~cwd:name
  end
    
end

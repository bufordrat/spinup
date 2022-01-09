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
 (name %s)
 (libraries prelude lib))
|} project_name project_name

  let gnumakefile =
    let top = ["# spinup                                 -*- makefile-gmake -*-"
              ; "# GNUmakefile"
              ; ""
              ; "DISPLAY = short"
              ; "DUNE = opam exec -- dune $1 --display $(DISPLAY)"
              ] in
    let rules = [
        "build all"                , None,         "build @@default";
        "test tests runtest check" , None,         "runtest";
        "install"                  , Some "build", "install";
        "doc"                      , None,         "build @doc";
        "clean"                    , None,         "clean";
      ] in
    let each (targets, deps, dune) =
      let deps' = match deps with
        | None      -> "::"
        | Some deps -> ": " ^ deps
      in
      [
        "";
        targets ^ deps';
        sprintf "\t$(call DUNE, %s)" dune;
        ".PHONY: " ^ targets
      ] |> join ~sep:"\n"
    in
    top @ map each rules @ [""] |> join ~sep:"\n"

end
include Constants

module Messages = struct
  let mk_project name =
    "creating " ^ name ^ "/ project..."

  let delete_bin =
    "removing bin/ directory..."

  let init_executable =
    "initializing executable project..."

  let create_lib =
    "creating library..."

  let create_lib_dune =
    "creating library dune config..."

  let create_exe name =
    "creating executable module " ^ name ^ ".ml..."

  let create_exe_dune =
    "creating executable dune config..."

  let create_use_output =
    "creating use-output for loading this project into the toplevel..."

  let create_ocamlinit =
    "creating minimal .ocamlinit file..."

  let create_dune_project =
    "creating dune-project file"

  let create_gnumakefile =
    "creating GNUmakefile"

  let done_msg = "DONE!"
end

module Paths = struct
  let lib_path = "lib/lib.ml"

  let lib_dune_path = "lib/dune"

  let dune_path = "dune"

  let use_output_path = "use-output.top"

  let ocamlinit_path = ".ocamlinit"

  let dune_project_path = "dune-project"

  let gnumakefile_path = "GNUmakefile"
end
include Paths

let keith = "hello"

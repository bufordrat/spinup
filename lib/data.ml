open Prelude

module Constants = struct
  let dune_project project_name = sprintf {|(lang dune 2.7)
(generate_opam_files true)
(package
 (name %s)
 (depends
  (ocaml (>= 4.13.0))
  dune
  prelude
  (utop :dev)
  (ocamlformat :dev)
  (ocp-index :dev)
  (merlin :dev)))
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
    "creating dune-project file..."

  let create_gnumakefile =
    "creating GNUmakefile..."

  let do_a_build =
    "doing initial `dune build` to generate .opam file..."

  let create_locked_file name =
    "creating " ^ name ^ ".opam.locked file for sandboxed opam switches..."
    
  let done_msg = "DONE!"

  let create_sandboxed_switch =
    "creating a sandboxed opam switch for the project..."

end

module Paths = struct
  let lib_path = "lib/lib.ml"

  let lib_dune_path = "lib/dune"

  let dune_path = "dune"

  let use_output_path = "use-output.top"

  let ocamlinit_path = ".ocamlinit"

  let dune_project_path = "dune-project"

  let gnumakefile_path = "GNUmakefile"

  let locked_path name = "./" ^ name ^ ".opam"
end
include Paths

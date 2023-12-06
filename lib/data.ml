module Constants = struct
  let dune_project name = Prelude.sprintf {|(lang dune 3.0)
 (generate_opam_files true)
 (package
  (name %s)
  (description "Insert project description here.")
  (synopsis "Insert project synopsis, which is supposedly different, here")
  (maintainers "Your Name <youremail@gmail.com>")
  (authors "Your Name <youremail@gmail.com>")
  (homepage "https://your.website.here")
  (bug_reports "https://your.website.here")
  (depends
   (ocaml (>= 4.14.0))
    dune
    camlp-streams
    prelude
    etude
    utop
    ocp-index
    merlin))
|} name

  let lib =
    "let message = \"GOODBYE CRUEL WORLD (is underrated)\""

  let lib_dune = {|(library
 (name lib)
 (libraries prelude etude))

 (env
   (dev
     (flags (:standard -warn-error -A))))
|}

  let exe = "let () = print_endline Lib.message"

  let exe_dune name = Prelude.sprintf {|(executable
 (public_name %s)
 (name %s)
 (promote (until-clean))
 (libraries camlp-streams prelude etude lib))

 (env
   (dev
     (flags (:standard -warn-error -A))))
|} name name

  (* let exe_dune_trac () =
   *   Prelude.readfile  *)

  module MakeFile = struct
    let top name = ["# " ^ name ^ "                   -*- makefile-gmake -*-"
                   ; "# GNUmakefile"
                   ; ""
                   ; "DISPLAY = short"
                   ; "DUNE = opam exec -- dune $1 --display $(DISPLAY)"
                   ]
    let sandbox = "opam switch create . --deps-only --repos dldc=https://dldc.lib.uchicago.edu/opam,default --yes"
    let repo = "opam repository add dldc https://dldc.lib.uchicago.edu/opam"
    let deps = "opam install . --deps-only --yes"

    let dune_rules = 
      let rules = [
          "build all"                , None,         "build @@default";
          "install"                  , Some "build", "install";
          "doc"                      , None,         "build @doc";
          "clean"                    , None,         "clean";
        ]
      in
      let open Prelude in
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
      map each rules

    let opam_rules =
      let open Prelude in
      let rules = [
          "sandbox", [sandbox], None ;
          "deps", [repo; deps], None ;
        ]
      in
      let tab s = "\t" ^ s in
      let each_script cmds =
        String.join ~sep:"\n" (map tab cmds)
      in
      let each_rule (targets, cmds, deps) =
        let deps = match deps with
          | None -> "::"
          | Some deps -> ": " ^ deps
        in
        [
          "";
          targets ^ deps ;
          each_script cmds ;
          "PHONY: " ^ targets ;
        ] |> join ~sep:"\n"
      in
      map each_rule rules @ [""]

    let gnumakefile name =
      top name @ dune_rules @ opam_rules |> Prelude.join ~sep:"\n"
  end
  let gnumakefile = MakeFile.gnumakefile

end
include Constants

module Messages = struct
  let mk_project name =
    "creating " ^ name ^ " project..."

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

  let do_a_clean =
    "doing a `dune clean` to remove compiler detritus..."

  let done_msg = "DONE!"

  let sandbox_msg name =
    Prelude.sprintf "\nto install project dependencies into the current opam \
             switch, run this command inside the %s/ directory:\n\
             \n\
             \  $ make deps\n\
             \n\
             to create a sandboxed opam switch, run this command \
             inside the %s/ directory:\n\
             \n\
             \  $ make sandbox\n" name name
end

module Paths = struct
  let lib_path = "lib/lib.ml"

  let lib_dune_path = "lib/dune"

  let dune_path = "dune"

  let ocamlinit_path = ".ocamlinit"

  let dune_project_path = "dune-project"

  let gnumakefile_path = "GNUmakefile"

  let locked_path name = "./" ^ name ^ ".opam"
end
include Paths

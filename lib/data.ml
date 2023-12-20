let get_ok = Stdlib.Result.get_ok


module Command = struct

end

module Action = struct

end

module Constants = struct
  let process_template = Template.Engine.process_template
  
  let dune_project name =
    process_template
      ~template:"dune_project"
      ~pname:name
    |> get_ok

  let lib = process_template
              ~template:"lib.ml"
              ~pname:""
            |> get_ok

  let lib_dune = process_template
                   ~template:"lib_dune"
                   ~pname:""
                 |> get_ok

  let exe = process_template
              ~template:"exe.ml"
              ~pname:""
            |> get_ok

  let exe_dune name = process_template
                        ~template:"exe_dune"
                        ~pname:name
                      |> get_ok

  let gnumakefile project_name =
    process_template
      ~template:"GNUMakefile"
      ~pname:project_name
    |> get_ok
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

  (* let create_ocamlinit =
   *   "creating minimal .ocamlinit file..." *)

  let create_dune_project =
    "creating dune-project file..."

  let create_gnumakefile =
    "creating GNUmakefile..."

  let do_a_build =
    "doing initial `dune build` to generate .opam file..."

  (* let create_locked_file name =
   *   "creating " ^ name ^ ".opam.locked file for sandboxed opam switches..." *)

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

  (* let ocamlinit_path = ".ocamlinit" *)

  let dune_project_path = "dune-project"

  let gnumakefile_path = "GNUmakefile"

  let locked_path name = "./" ^ name ^ ".opam"
end
include Paths

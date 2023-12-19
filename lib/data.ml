let get_ok = Stdlib.Result.get_ok

module Unvalidated = struct
  type t = { filename : string ;
             input_path : string ;
             output_path : string ;
             context : (string * string) list ;
             umessage : string ;
           }

  let debug_string t =
    let open Prelude in
    let context_string context =
      let each_keypair (k,v) = "    " ^ k ^ ": " ^ v in
      String.join ~sep:"\n" (List.map each_keypair context)
    in
    let fields = [ "Filename: ", t.filename ;
                   "Input path: ", t.input_path ;
                   "Output path: ", t.output_path ;
                   "Context: ", context_string t.context ;
                   "Message: ", t.umessage ; ] in
    let each_field (name, content) = name ^ content in
    let field_string =
      String.join ~sep:"\n" (List.map each_field fields)
    in "Unvalidated!\n" ^ field_string

  let debug_print t = Prelude.print (debug_string t)
end
  
module Validated = struct
  type valid = { write_path : string ;
                 data : string ;
                 vmessage : string }

  type 'a t = (valid, string) result

  let debug_string valid =
    let open Prelude in
    let fields = [ "Write path: ", valid.write_path ;
                   "Data : ", String.take 30 valid.data ^ "..." ;
                   "Message: ", valid.vmessage ; ] in
    let each_field (name, content) = name ^ content in
    let field_string =
      String.join ~sep:"\n" (List.map each_field fields)
    in "Validated!\n" ^ field_string

  let debug_print = function
    | Ok valid -> Prelude.print (debug_string valid)
    | Error msg -> Prelude.print ("Error!\n" ^ msg)
end

module Templates = struct
  let ipath = Template.path

  let dune_project name =
    let open Unvalidated in 
    { filename = "dune-project" ;
      input_path = ipath ;
      output_path = "./" ;
      context = [ "pname", name ] ;
      umessage = "creating dune-project file..." ; }
end


module Constants = struct
  let process_template = Template.process_template
  
  let dune_project name =
    process_template
      ~template:"dune_project"
      ~pname:name
    |> get_ok

  let lib = process_template
              ~template:"lib"
              ~pname:""
            |> get_ok

  let lib_dune = process_template
                   ~template:"lib_dune"
                   ~pname:""
                 |> get_ok

  let exe = process_template
              ~template:"exe"
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

  let create_ocamlinit =
    "creating minimal .ocamlinit file..."

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

let get_ok = Stdlib.Result.get_ok

module Input = struct
  type t = { input_filename : string ;
             output_filename : string ;
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
    let fields = [ "Input filename: ", t.input_filename ;
                   "Output filename: ", t.output_filename ;
                   "Input path: ", t.input_path ;
                   "Output path: ", t.output_path ;
                   "Context: ", "\n" ^ context_string t.context ;
                   "Message: ", t.umessage ; ] in
    let each_field (name, content) = name ^ content in
    let field_string =
      String.join ~sep:"\n" (List.map each_field fields)
    in "Unvalidated!\n" ^ field_string

  let debug_print t = Prelude.print (debug_string t)
end
  
module Output = struct
  type valid = { write_path : string ;
                 data : string ;
                 vmessage : string }

  type t = (valid, string) result

  let debug_string valid =
    let open Prelude in
    let fields = [ "Write path: ", valid.write_path ;
                   "Data: ", String.take 50 valid.data ^ "..." ;
                   "Message: ", valid.vmessage ; ] in
    let each_field (name, content) = name ^ content in
    let field_string =
      String.join ~sep:"\n" (List.map each_field fields)
    in "Validated!\n" ^ field_string

  let debug_print = function
    | Ok valid -> Prelude.print (debug_string valid)
    | Error msg -> Prelude.print ("Error!\n" ^ msg)

  let validate unv =
    let open Input in
    let open Etude.Result.Make (String) in
    let context = unv.context in
    let write_path =
      unv.output_path ^ "/" ^ unv.output_filename in
    let vmessage = unv.umessage in
    let+ data = Template.process_template'
                  ~template:unv.input_filename
                  ~context:context
    in { write_path ; data ; vmessage }
end

module Templates = struct
  let ipath = Template.Path.path

  let dune_project name =
    let open Input in 
    { input_filename = "dune-project" ;
      output_filename = "dune-project" ;
      input_path = ipath ;
      output_path = "./" ;
      context = [ "pname", name ] ;
      umessage = "creating dune-project file..." ; }

  let lib () =
    let open Input in
    { input_filename = "lib.ml" ;
      output_filename = "lib.ml" ;
      input_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library..." ; }

  let lib_dune () =
    let open Input in
    { input_filename = "lib_dune" ;
      output_filename = "dune" ;
      input_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library dune config..." ; }
end

module Constants = struct
  let process_template = Template.process_template
  
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

module Path = struct
  let path = "/home/teichman/.config/spinup/templates"
end

module Engine = struct
  let default_syntax = Tint.Types.Syntax.tracstring

  let context_to_state ?(syntax=default_syntax) context =
    let open Etude.Result.Make (String) in
    let* syntax = Tint.Types.Syntax.of_string syntax in
    Tint.Eval.(init ~syntax prims (Forms.forms context))

  let macro_expand ?syntax ~context tint =
    let open Etude.Result.Make (String) in
    let* state = context_to_state ?syntax context in
    let+ (_, processed_string) =
      map_error
        (Tint.Types.error_message state.syntax)
        (Tint.Eval.eval state tint)
    in processed_string

  (* eventually change process_template_path to this *)
  let process_template_path path template context =
    let open Etude.Result.Make (String) in
    let fullpath = path ^ "/" ^ template in
    let* raw_contents =
      let open Prelude in
      trap Exn.to_string readfile fullpath
    in
    macro_expand ~syntax:"#[,]" ~context raw_contents

  let process_template ~template ~context =
    process_template_path Path.path template context
end

module Unprocessed = struct
  type t = { template_filename : string ;
             output_filename : string ;
             template_path : string ;
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
    let fields = [ "Template filename: ", t.template_filename ;
                   "Output filename: ", t.output_filename ;
                   "Template path: ", t.template_path ;
                   "Output path: ", t.output_path ;
                   "Context: ", "\n" ^ context_string t.context ;
                   "Message: ", t.umessage ; ] in
    let each_field (name, content) = name ^ content in
    let field_string =
      String.join ~sep:"\n" (List.map each_field fields)
    in "Unprocessed!\n" ^ field_string

  let debug_print t = Prelude.print (debug_string t)
end
  
module Processed = struct
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
    in "Processed!\n" ^ field_string

  let debug_print valid = Prelude.print (debug_string valid)

  let process unv =
    let open Unprocessed in
    let open Etude.Result.Make (String) in
    let context = unv.context in
    let write_path =
      unv.output_path ^ "/" ^ unv.output_filename in
    let vmessage = unv.umessage in
    let+ data = Engine.process_template
                  ~template:unv.template_filename
                  ~context:context
    in { write_path ; data ; vmessage }

  let write { write_path ; data ; vmessage } =
    let open Prelude in
    print vmessage ;
    writefile ~fn:write_path data
end

module FromFiles = struct
  let ipath = Path.path

  (* project root files *)

  let dune_project name =
    Unprocessed. 
    { template_filename = "dune-project" ;
      output_filename = "dune-project" ;
      template_path = ipath ;
      output_path = "." ;
      context = [ "pname", name ] ;
      umessage = "creating dune-project file..." ; }

  let gnumakefile name =
    Unprocessed.
    { template_filename = "GNUmakefile" ;
      output_filename = "GNUmakefile" ;
      template_path = ipath ;
      output_path = "." ;
      context = [ "pname", name ] ;
      umessage = "creating GNUmakefile..." ; }

  (* app/ files *)

  let app_dune name =
    Unprocessed.
    { template_filename = "app_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./app" ;
      context = [ "pname", name ] ;
      umessage = "creating app/dune file..." ; }

  let app_exe name =
    Unprocessed.
    { template_filename = "project.ml" ;
      output_filename = name ^ ".ml" ;
      template_path = ipath ;
      output_path = "./app" ;
      context = [] ;
      umessage = "creating executable module app/" ^ name ^ ".ml..." ; }

  (* lib/ files *)

  let lib_dune () =
    Unprocessed.
    { template_filename = "lib_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library dune config..." ; }

  let lib_lib () =
    Unprocessed.
    { template_filename = "lib.ml" ;
      output_filename = "lib.ml" ;
      template_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library..." ; }

  (* test/ files *)

  let test_dune name =
    Unprocessed.
    { template_filename = "test_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./test" ;
      context = [ "pname", name ] ;
      umessage = "creating test/dune file..." ; }

  let test_test name =
    Unprocessed.
    { template_filename = "test_project.ml" ;
      output_filename = "test_" ^ name ^ ".ml" ;
      template_path = ipath ;
      output_path = "./test" ;
      context = [] ;
      umessage = "creating test/test_" ^ name ^ ".ml file..." ; }

  (* all the files, Katie *)

  let files name = [
      dune_project name ;
      gnumakefile name ;
      app_dune name ;
      app_exe name ;
      lib_dune () ;
      lib_lib () ;
      test_dune name ;
      test_test name ;
    ]
end


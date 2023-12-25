type t =
  | Write of Template.Processed.valid
  | Run of Command.t

let run = function
  | Write tmpl -> Template.Processed.write tmpl
  | Run cmd -> Command.run cmd

let dry_run =
  function
  | Write tmpl ->
     let output = "    WRITE    " ^ tmpl.write_path
     in print_endline output
  | Run { args = [] ; _ } -> ()
  | Run cmd ->
     let open Prelude.String in
     let output = "      RUN    " ^ join ~sep:" " cmd.args
     in print_endline output

let debug_print = function
  | Write tmpl -> Template.Processed.debug_print tmpl
  | Run cmd -> Command.debug_print cmd

let write v = Write v

module Opening = struct
  let mk_projectdir name =
    Run Command.
    { args = [] ;
      cmessage = "making " ^ name ^ "/ directory..." ; }
end

module Dirs = struct
  let mk_appdir =
    Run Command.
    { args = [ "mkdir" ; "app" ] ;
      cmessage = "making app/ directory..." ; }

  let mk_libdir =
    Run Command.
    { args = [ "mkdir" ; "lib" ] ;
      cmessage = "making lib/ directory..." ; }

  let mk_testdir =
    Run Command.
    { args = [ "mkdir" ; "test" ] ;
      cmessage = "making test/ directory..." ; }
end

module Files = struct
  let ipath = Template.Path.path

  let dune_project name =
    Template.Unprocessed. 
    { template_filename = "dune-project" ;
      output_filename = "dune-project" ;
      template_path = ipath ;
      output_path = "." ;
      context = [ "pname", name ] ;
      umessage = "creating dune-project file..." ; }

  let gnumakefile name =
    Template.Unprocessed.
    { template_filename = "GNUmakefile" ;
      output_filename = "GNUmakefile" ;
      template_path = ipath ;
      output_path = "." ;
      context = [ "pname", name ] ;
      umessage = "creating GNUmakefile..." ; }

  let app_dune name =
    Template.Unprocessed.
    { template_filename = "app_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./app" ;
      context = [ "pname", name ] ;
      umessage = "creating app/dune file..." ; }

  let app_exe name =
    Template.Unprocessed.
    { template_filename = "project.ml" ;
      output_filename = name ^ ".ml" ;
      template_path = ipath ;
      output_path = "./app" ;
      context = [] ;
      umessage = "creating executable module app/" ^ name ^ ".ml..." ; }

  let lib_dune () =
    Template.Unprocessed.
    { template_filename = "lib_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library dune config..." ; }

  let lib_lib () =
    Template.Unprocessed.
    { template_filename = "lib.ml" ;
      output_filename = "lib.ml" ;
      template_path = ipath ;
      output_path = "./lib" ;
      context = [] ;
      umessage = "creating library..." ; }

  let test_dune name =
    Template.Unprocessed.
    { template_filename = "test_dune" ;
      output_filename = "dune" ;
      template_path = ipath ;
      output_path = "./test" ;
      context = [ "pname", name ] ;
      umessage = "creating test/dune file..." ; }

  let test_test name =
    Template.Unprocessed.
    { template_filename = "test_project.ml" ;
      output_filename = "test_" ^ name ^ ".ml" ;
      template_path = ipath ;
      output_path = "./test" ;
      context = [] ;
      umessage = "creating test/test_" ^ name ^ ".ml file..." ; }

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

module Conclude = struct
  let do_a_build =
    Run Command.
    { args = [ "dune" ; "build" ] ;
      cmessage =
        "doing initial `dune build` to generate .opam file..." ; }

  let do_a_clean =
    Run Command.
    { args = [ "dune" ; "clean" ] ;
      cmessage =
        "doing a `dune clean` to remove compiler detritus..." ; }

  let done_msg =
    Run Command.
    { args = [] ;
      cmessage = "DONE!" ; }

  let sandbox_msg name =
    let msg =
      Prelude.sprintf "\nto install project dependencies into the current opam \
                       switch, run this command inside the %s/ directory:\n\
                       \n\
                       \  $ make deps\n\
                       \n\
                       to create a sandboxed opam switch, run this command \
                       inside the %s/ directory:\n\
                       \n\
                       \  $ make sandbox\n" name name
    in
    Run Command.
    { args = [] ;
      cmessage = msg ; }
end

let main_actions name =
  let open Template in
  let open Etude.Result.Make (String) in
  let make_dirs = Dirs.[
        mk_appdir ;
        mk_libdir ;
        mk_testdir ; ]
  in
  let files = Files.files name in
  (* let* _ =
   *   traverse
   *     Unprocessed.check_exists
   *     files
   * in *)
  let+ processed =
    traverse
      Processed.process
      files
  in
  let writes =
    List.map write processed
  in  
  let finish_up = Conclude.[
        do_a_build ;
        do_a_clean ;
        done_msg ;
        sandbox_msg name ; ]
  in
  make_dirs @ writes @ finish_up


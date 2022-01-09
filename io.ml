open Prelude

module SmallCommands = struct
  open Lib
  open Unix.Proc

  module Verbosity = struct
    let verbose = true

    let verbose_print msg =
      if verbose
      then print msg
      else ()
  end
  open Verbosity

  let mk_project name =
    runfull [ "dune" ; "init" ; "project" ; name ]
    |> ignore ;
    verbose_print @@ Messages.mk_project name

  let delete_bin () =
    runfull [ "rm" ; "-R" ; "bin" ]
    |> ignore ;
    verbose_print Messages.delete_bin

  let init_executable name =
    runfull [ "dune" ; "init" ; "exe" ; name ]
    |> ignore ;
    verbose_print Messages.init_executable

  let create_lib () =
    writefile ~fn:lib_path lib;
    verbose_print Messages.create_lib

  let create_lib_dune () =
    writefile ~fn:lib_dune_path lib_dune;
    verbose_print Messages.create_lib_dune

  let create_exe name =
    writefile ~fn:(name ^ ".ml") exe;
    verbose_print @@ Messages.create_exe name

  let create_exe_dune name =
    writefile ~fn:dune_path (exe_dune name);
    verbose_print Messages.create_exe_dune

  let create_use_output () =
    writefile ~fn:use_output_path use_output;
    verbose_print Messages.create_use_output

  let create_ocamlinit () =
    writefile ~fn:ocamlinit_path ocamlinit;
    verbose_print Messages.create_ocamlinit

  let create_dune_project name =
    writefile ~fn:dune_project_path (dune_project name);
    verbose_print Messages.create_dune_project

  let create_gnumakefile () =
    writefile ~fn:gnumakefile_path gnumakefile;
    verbose_print Messages.create_gnumakefile

  let done_msg () = print Messages.done_msg
end
open SmallCommands

module BigPicture = struct
  let mk_project_root name =
    mk_project name

  let inside_the_dir name =
    delete_bin () ;
    init_executable name ;
    create_lib () ;
    create_lib_dune () ;
    create_exe name ;
    create_exe_dune name ;
    create_use_output () ;
    create_ocamlinit () ;
    create_dune_project name ;
    create_gnumakefile ();
    done_msg ()

  let the_whole_thing name =
    mk_project_root name
    ; withcd (fun _ -> inside_the_dir name) name
end
include BigPicture

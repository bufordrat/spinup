open Prelude

module type SETTINGS = sig
  val verbose : bool
end

module SmallCommands (S : SETTINGS) = struct
  open Data
  open Unix.Proc

  module Verbosity = struct
    let verbose_print msg =
      if S.verbose
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

  let create_dune_project name =
    writefile ~fn:dune_project_path (dune_project name);
    verbose_print Messages.create_dune_project

  let create_gnumakefile () =
    writefile ~fn:gnumakefile_path gnumakefile;
    verbose_print Messages.create_gnumakefile

  let do_a_build () =
    runfull [ "dune" ; "build" ]
    |> ignore ;
    verbose_print Messages.do_a_build

  let create_locked_file name =
    let full_command = [ "opam" ; "lock" ; locked_path name ] in
    runfull ~err:(print_endline << input_line) full_command
    |> ignore ;
    verbose_print @@ Messages.create_locked_file name

  let create_sandboxed_switch () =
    let command = "opam" in
    let subcommand = [ "switch" ; "create" ] in
    let path = "." in
    let switches = [ "--deps-only" ; "--locked" ; "--yes" ] in
    let options =
      [ "--repos" ;
        "dldc=https://dldc.lib.uchicago.edu/opam,default" ] in
    let full_command =
      command :: subcommand @ path :: switches @ options
    in
    runfull ~err:(print_endline << input_line) full_command
    |> ignore ;
    verbose_print @@ Messages.create_sandboxed_switch

  let done_msg () = print Messages.done_msg
end

module BigPicture (S : SETTINGS) = struct
  open SmallCommands (S)

  let mk_project_root name =
    mk_project name

  let inside_the_dir name =
    delete_bin () ;
    init_executable name ;
    create_lib () ;
    create_lib_dune () ;
    create_exe name ;
    create_exe_dune name ;
    create_dune_project name ;
    create_gnumakefile () ;
    do_a_build () ;
    create_locked_file name ;
    create_sandboxed_switch () ;
    done_msg ()

  let the_whole_thing name =
    mk_project_root name
    ; withcd (fun _ -> inside_the_dir name) name
end


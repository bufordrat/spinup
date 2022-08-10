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

  module RunHelpers = struct
    let run_it args msg =
      runfull
        ~err:Prelude.(prerr_endline << read)
        ~reader:Prelude.(ignore << read)
        args
      |> ignore ;
      verbose_print msg

    let write_it path data msg =
      writefile ~fn:path data ;
      verbose_print msg
  end
  open RunHelpers

  let mk_project name =
    run_it
      [ "dune" ; "init" ; "project" ; name ]
      (Messages.mk_project name)

  let delete_bin () =
    run_it
      [ "rm" ; "-R" ; "bin" ]
      Messages.delete_bin

  let init_executable name =
    run_it
      [ "dune" ; "init" ; "exe" ; name ]
      Messages.init_executable

  let create_lib () =
    write_it
      lib_path
      lib
      Messages.create_lib

  let create_lib_dune () =
    write_it
      lib_dune_path
      lib_dune
      Messages.create_lib_dune

  let create_exe name =
    write_it
      (name ^ ".ml")
      exe
      (Messages.create_exe name)

  let create_exe_dune name =
    write_it
      dune_path
      (exe_dune name)
      Messages.create_exe_dune

  let create_dune_project name =
    write_it
      dune_project_path
      (dune_project name)
      Messages.create_dune_project

  let create_gnumakefile () =
    write_it
      gnumakefile_path
      gnumakefile
      Messages.create_gnumakefile

  let do_a_build () =
    run_it
      [ "dune" ; "build" ]
      Messages.do_a_build

  let create_locked_file name =
    let full_command = [ "opam" ; "lock" ; locked_path name ] in
    run_it full_command (Messages.create_locked_file name)

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
    run_it full_command Messages.create_sandboxed_switch

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


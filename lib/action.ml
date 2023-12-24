type action =
  | Write of Template.Processed.valid
  | Run of Command.t

let run = function
  | Write tmpl -> Template.Processed.write tmpl
  | Run cmd -> Command.run cmd

let is_write = function
  | Write _ -> true
  | _ -> false

let write v = Write v

type t = (action, string) result

module Dirs = struct
  let mk_appdir =
    Command.
    { args = [ "mkdir" ; "app" ] ;
      cmessage = "making app/ directory..." ; }

  let mk_libdir =
    Command.
    { args = [ "mkdir" ; "lib" ] ;
      cmessage = "making lib/ directory..." ; }

  let mk_testdir =
    Command.
    { args = [ "mkdir" ; "test" ] ;
      cmessage = "making test/ directory..." ; }
end

let mk_project name =
  Command.
  { args = [ "dune" ; "init" ; "project" ; name ] ;
    cmessage = "creating " ^ name ^ " project..." ; }

let delete_bin =
  Command.
  { args = [ "rm" ; "-r" ; "bin" ] ;
    cmessage = "creating library dune config..." ; }

let init_executable name =
  Command.
  { args = [ "dune" ; "init" ; "exe" ; name ] ;
    cmessage =
      "creating executable module " ^ name ^ ".ml..." ; }

let do_a_build =
  Command.
  { args = [ "dune" ; "build" ] ;
    cmessage =
      "doing initial `dune build` to generate .opam file..." ; }

let do_a_clean =
  Command.
  { args = [ "dune" ; "clean" ] ;
    cmessage =
      "doing a `dune clean` to remove compiler detritus..." ; }

let done_msg =
  Command.
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
  Command.
  { args = [] ;
    cmessage = msg ; }

(* TODO: fix this so that the file writes are processed *)

let main_actions name =
  let open Template in
  let open Etude.Result.Make (String) in
  let dune_commands1 = [
      Run delete_bin ;
      Run (init_executable name) ;
    ]
  in
  let dune_commands2 = [
      Run do_a_build ;
      Run do_a_clean ;
      Run done_msg ;
      Run (sandbox_msg name) ; ]
  in
  let+ processed =
    traverse Processed.process (FromFiles.files name)
  in
  let writes =
    List.map write processed
  in
  dune_commands1 @ writes @ dune_commands2

(* let inside_the_dir name =
 *   let open Template.FromFiles in
 *   let dune_commands1 = [
 *       Run delete_bin ;
 *       Run (init_executable name) ;
 *     ] inp
 *   let file_writes =
 *     List.map write (files name)
 *   in
 *   let dune_commands2 = [
 *       Run do_a_build ;
 *       Run do_a_clean ;
 *       Run done_msg ;
 *       Run (sandbox_msg name) ; ]
 *   in
 *   dune_commands1 @ file_writes @ dune_commands2 *)
      

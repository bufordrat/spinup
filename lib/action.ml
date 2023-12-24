type action =
  | Write of Template.Processed.valid
  | Run of Command.t

let run = function
  | Write tmpl -> Template.Processed.write tmpl
  | Run cmd -> Command.run cmd

let debug_print = function
  | Write tmpl -> Template.Processed.debug_print tmpl
  | Run cmd -> Command.debug_print cmd

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

module Conclude = struct
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
end

let main_actions name =
  let open Template in
  let open Etude.Result.Make (String) in
  let make_dirs = Dirs.[
        Run mk_appdir ;
        Run mk_libdir ;
        Run mk_testdir ; ]
  in
  let+ processed =
    traverse
      Processed.process
      (FromFiles.files name)
  in
  let writes =
    List.map write processed
  in  
  let finish_up = Conclude.[
        Run do_a_build ;
        Run do_a_clean ;
        Run done_msg ;
        Run (sandbox_msg name) ; ]
  in
  make_dirs @ writes @ finish_up
      

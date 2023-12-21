type action =
  | Write of Template.Processed.valid
  | Run of Command.t

let run = function
  | Write tmpl -> Template.Processed.write tmpl
  | Run cmd -> Command.run cmd

let is_write = function
  | Write _ -> true
  | _ -> false

type t = (action, string) result

let mk_project name =
  Command.
  { args = [ "dune" ; "init" ; "project" ; name ] ;
    cmessage = "creating " ^ name ^ " project..." ; }

let delete_bin () =
  Command.
  { args = [ "rm" ; "-r" ; "bin" ] ;
    cmessage = "creating library dune config..." ; }

let init_executable name =
  Command.
  { args = [ "dune" ; "init" ; "exe" ; name ] ;
    cmessage =
      "creating executable module " ^ name ^ ".ml..." ; }

let do_a_build () =
  Command.
  { args = [ "dune" ; "build" ] ;
    cmessage =
      "doing initial `dune build` to generate .opam file..." ; }

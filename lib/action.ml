module R = Etude.Result.Make (String)

type dir = { dir : string ;
             actions : t list ; }

and t =
  | Write of Template.Processed.t
  | Run of Command.t
  | WithCD of dir

module Opening = struct
  let mk_projectdir name =
    Run Command.
    { args = [ "mkdir" ; name ] ;
      cmessage = "making " ^ name ^ "/ directory..." ; }
end

let rec run = function
  | Write tmpl ->
     Template.Processed.write tmpl
  | Run cmd -> Command.run cmd
  | WithCD d ->
     let handler _ =
       List.iter run d.actions
     in
     run (Opening.mk_projectdir d.dir) ;
     Prelude.(withcd handler d.dir)

let rec dry_run =
  function
  | Write tmpl ->
     let output =
       "    WRITE    " ^ tmpl.write_path
     in print_endline output
  | Run { args = [] ; _ } -> ()
  | Run cmd ->
     let open Prelude.String in
     let output =
       "      RUN    " ^ join ~sep:" " cmd.args
     in print_endline output
  | WithCD d ->
     let msg = "      RUN    cd " ^ d.dir in
     begin
       dry_run (Opening.mk_projectdir d.dir) ;
       print_endline msg ;
       List.iter dry_run d.actions
     end

let write v = Write v

module Dirs = struct
  let dirnames flist =
    let open Stdlib.Filename in
    let not_dot dir = dir <> "." in
    List.map dirname flist
    |> List.filter not_dot

  let dir_to_action dir = 
    Run Command.
    { args = [ "mkdir" ; "-p" ; dir ] ;
      cmessage = "making " ^ dir ^ "/ directory..." ; }

  let dirs () =
    let ds = dirnames Crunched_templates.file_list in
    List.map dir_to_action ds
end

module Files = struct
  let from_files pname path = 
    let open Template.Unprocessed in
    let open Stdlib.Filename in
    let dirname path =
      match Stdlib.Filename.dirname path with
      | "." -> ""
      | p -> p
    in
    { template_filename = basename path ;
      output_filename = basename path ;
      template_path = "" ;
      output_path = dirname path ;
      context = [ "pname", pname ] ;
      umessage = "creating " ^ path ^ " file..." ; } 

  let files pname =
    let open Prelude in
    let lst =
      List.delete ".dir-locals.el" Crunched_templates.file_list
    in
    List.map (from_files pname) lst
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

  let sandbox_msg pname =
    let msg =
      Prelude.sprintf "\nto install project dependencies into the current opam \
                       switch, run this command inside the %s/ directory:\n\
                       \n\
                       \  $ make deps\n\
                       \n\
                       to create a sandboxed opam switch, run this command \
                       inside the %s/ directory:\n\
                       \n\
                       \  $ make sandbox\n" pname pname
    in
    Run Command.
    { args = [] ;
      cmessage = msg ; }
end

let directory_actions pname =
  let open Template in
  let open R in
  let dirs = Dirs.dirs () in
  let files = Files.files pname in
  let+ processed =
    traverse
      Unprocessed.process
      files
  in
  let writes =
    List.map write processed
  in  
  let finish_up = Conclude.[
        do_a_build ;
        do_a_clean ;
        done_msg ;
        sandbox_msg pname ; ]
  in
  dirs @ writes @ finish_up

let main_action pname =
  let open R in
  let* () = Errors.already_exists pname in
  let+ actions = directory_actions pname in
  WithCD { dir = pname ;
           actions = actions ; }

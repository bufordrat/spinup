module E = Action_error
module R = Etude.Result.Make (Global_error)
module Trace = Global_error.T

module Command = struct
  type t = { args : string list; cmessage : string }

  let run { args; cmessage } =
    let open Prelude in
    let open Unix.Proc in
    match args with
    | [] -> print cmessage
    | _ ->
      print cmessage ;
      runfull
        ~err:Prelude.(ignore << read)
        ~reader:Prelude.(ignore << read)
        args
      |> ignore
end

type dir =
  { dir : string; actions : t list; config : Config.t }

and t =
  | Write of Template.Processed.t
  | Print of string
  | Run of Command.t
  | WithCD of dir

module Main = struct
  module Opening = struct
    let say_which_config config =
      let open Config in
      let msg =
        match config.datasource with
        | FromAFile path -> "using config file at: " ^ path
        | _ -> "using default config..."
      in
      Print msg

    let mk_projectdir name =
      Run
        Command.
          { args = [ "mkdir"; name ];
            cmessage = "making " ^ name ^ "/ directory..."
          }
  end

  let rec run = function
    | Write tmpl -> Template.Processed.write tmpl
    | Run cmd -> Command.run cmd
    | Print msg -> print_endline msg
    | WithCD d ->
      let handler _ = List.iter run d.actions in
      run (Opening.say_which_config d.config) ;
      run (Opening.mk_projectdir d.dir) ;
      Prelude.(withcd handler d.dir)

  let rec dry_run = function
    | Write tmpl ->
      let output = "    WRITE    " ^ tmpl.write_path in
      print_endline output
    | Print msg ->
      let open Prelude.String in
      let trimmed =
        if length msg <= 200
        then msg
        else (trim whitespace) (take 41 msg) ^ "... etc."
      in
      let output = "    PRINT    " ^ trimmed in
      print_endline output
    | Run cmd ->
      let open Prelude.String in
      let output =
        "      RUN    " ^ join ~sep:" " cmd.args
      in
      print_endline output
    | WithCD d ->
      let msg = "      RUN    cd " ^ d.dir in
      dry_run (Opening.say_which_config d.config) ;
      dry_run (Opening.mk_projectdir d.dir) ;
      print_endline msg ;
      List.iter dry_run d.actions

  let write v = Write v

  module Dirs = struct
    let dirnames flist =
      let open Stdlib.Filename in
      let not_dot dir = dir <> "." in
      List.map dirname flist |> List.filter not_dot

    let dir_to_action dir =
      Run
        Command.
          { args = [ "mkdir"; "-p"; dir ];
            cmessage = "making " ^ dir ^ "/ directory..."
          }

    let dirs () =
      let ds = dirnames Crunched_templates.file_list in
      List.map dir_to_action ds
  end

  module Files = struct
    let from_files config template_path =
      let open Template.Unprocessed in
      let open Stdlib.Filename in
      let dirname template_path =
        match Stdlib.Filename.dirname template_path with
        | "." -> ""
        | p -> p
      in
      { template_filename = basename template_path;
        output_filename = basename template_path;
        template_path = "";
        output_path = dirname template_path;
        context = Config.(config.context);
        umessage = "creating " ^ template_path ^ " file..."
      }

    let files config =
      let open Prelude in
      let lst =
        List.delete ".dir-locals.el"
          Crunched_templates.file_list
      in
      List.map (from_files config) lst
  end

  module Conclude = struct
    let do_a_build =
      Run
        Command.
          { args = [ "dune"; "build" ];
            cmessage =
              "doing initial `dune build` to generate \
               .opam file..."
          }

    let do_a_clean =
      Run
        Command.
          { args = [ "dune"; "clean" ];
            cmessage =
              "doing a `dune clean` to remove compiler \
               detritus..."
          }

    let done_msg = Print "DONE!"

    let sandbox_msg config =
      let msg =
        let open Config in
        Prelude.sprintf
          "\n\
           to install project dependencies into the \
           current opam switch, run this command inside \
           the %s/ directory:\n\n\
          \ $ make deps\n\n\
           to create a sandboxed opam switch, run this \
           command inside the %s/ directory:\n\n\
          \ $ make sandbox\n"
          config.pname config.pname
      in
      Print msg
  end

  let directory_actions config =
    let open Template.Unprocessed in
    let open R in
    let dirs = Dirs.dirs () in
    let files = Files.files config in
    let template_err = E.Smart.template_err in
    let+ processed =
      Trace.with_error template_err (traverse process files)
    in
    let writes = List.map write processed in
    let finish_up =
      Conclude.
        [ do_a_build;
          do_a_clean;
          done_msg;
          sandbox_msg config
        ]
    in
    dirs @ writes @ finish_up

  let action pname =
    let open R in
    let open E.Smart in
    let open Trace in
    let pname_ok = Filesystem.validate_project_name pname in
    let already_exists = Filesystem.already_exists pname in
    let prereqs_ok = Filesystem.(check_prereqs prereqs) in
    let* () = with_error filesystem_err pname_ok in
    let* () = with_error filesystem_err already_exists in
    let* () = with_error filesystem_err prereqs_ok in
    let* config =
      with_error config_err
        Config.(get_config pname default_paths)
    in
    let+ actions = directory_actions config in
    WithCD { dir = pname; actions; config }
end

module PrintConfig = struct
  let print_config =
    let open R in
    let open Config in
    let+ config_string =
      Crunch.get_raw_config crunch_path
    in
    Print config_string
end

module Message = struct
  type application_layer = Config | Template | Filesystem

  type t =
    | ReferError of
        application_layer * Datasource.t * int * string
    | CrunchedConfigPath of
        Lineinfo.t * application_layer * string
    | FileReadError of application_layer * string
    | AlreadyExists of
        application_layer
        * string
        * Filesystem_error.dir_or_file
        * string
    | BadProjectName of application_layer * string
    | MissingPrereqs of application_layer * string list
    | TintSyntaxRecord of Lineinfo.t * string
    | TintSyntaxError of
        Datasource.t
        * application_layer
        * Template_error.tint_syntax
    | TemplateCrunch of string
    | ParseError of
        string * Global_error.error option * Global_error.t
end

module R = Etude.Result.Make (Message)

module Parser = struct
  module P = struct
    type 'a t =
      Global_error.t ->
      ('a * Global_error.t, Message.t) result
  end

  module ParserMonad = struct
    type 'a t = 'a P.t

    let pure a input = Ok (a, input)

    let bind mx k input =
      let open R in
      let* result, remainder = mx input in
      (k result) remainder
  end

  include Etude.Endofunctors.Monad.Make (ParserMonad)

  module ParserAlternative = struct
    type 'a t = 'a P.t

    let empty _ =
      let open Message in
      Error (ParseError ("fail parser", None, []))

    let append prsr1 prsr2 input =
      match prsr1 input with
      | Ok o -> Ok o
      | Error _ -> prsr2 input
  end

  include Etude.Monoid.Make (ParserAlternative)

  let eval prsr input = R.map fst (prsr input)

  let satisfy pred =
    let open Message in
    function
    | tok :: toks ->
      if pred tok
      then Ok (tok, toks)
      else Error (ParseError ("satisfy", Some tok, toks))
    | [] -> Error (ParseError ("end of input", None, []))

  let eof input =
    let open Message in
    match input with
    | [] -> Ok ((), [])
    | tok :: toks ->
      Error (ParseError ("eof", Some tok, toks))
end

module Parsers = struct
  [@@@warning "-8"]

  let config_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ _ = satisfy is_config_err in
    Config

  let template_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ _ = satisfy is_template_err in
    Template

  let filesystem_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ _ = satisfy is_filesystem_err in
    Filesystem

  let application_layer_parser =
    let open Parser in
    config_parser <|> template_parser <|> filesystem_parser

  let refer_error_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = application_layer_parser
    and+ (`ReferError (datasource, line, msg)) =
      satisfy is_refer_error
    in
    ReferError (layer, datasource, line, msg)

  let crunched_config_path_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = config_parser
    and+ (`BadCrunchPath (path, lineinfo)) =
      satisfy is_bad_crunch_path
    in
    CrunchedConfigPath (lineinfo, layer, path)

  let file_read_error_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = application_layer_parser
    and+ (`FileReadError system_msg) =
      satisfy is_file_read_error
    in
    FileReadError (layer, system_msg)

  let already_exists_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = filesystem_parser
    and+ (`AlreadyExists (cwd, dir_or_file, pname)) =
      satisfy is_already_exists
    in
    AlreadyExists (layer, cwd, dir_or_file, pname)

  let bad_project_name_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = filesystem_parser
    and+ (`BadProjectName pname) =
      satisfy is_bad_project_name
    in
    BadProjectName (layer, pname)

  let missing_prereqs_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = filesystem_parser
    and+ (`MissingPrereqs binaries) =
      satisfy is_missing_prereqs
    in
    MissingPrereqs (layer, binaries)

  let tint_syntax_record_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ _ = application_layer_parser
    and+ (`BadSyntaxRecord (lineinfo, msg)) =
      satisfy is_bad_syntax_record
    in
    TintSyntaxRecord (lineinfo, msg)

  let tint_syntax_error_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ layer = application_layer_parser
    and+ (`TintSyntaxError ({ path; _ } as ts)) =
      satisfy is_tint_syntax_error
    in
    TintSyntaxError (FromCrunch path, layer, ts)

  let template_crunch_parser =
    let open Parser in
    let open Message in
    let open Global_error.Smart in
    let+ _ = application_layer_parser
    and+ (`TemplateCrunch path) =
      satisfy is_template_crunch
    in
    TemplateCrunch path

  let global_error_parser =
    let open Parser in
    let+ result =
      refer_error_parser
      <|> crunched_config_path_parser
      <|> file_read_error_parser
      <|> already_exists_parser
      <|> bad_project_name_parser
      <|> missing_prereqs_parser
      <|> tint_syntax_record_parser
      <|> tint_syntax_error_parser
      <|> template_crunch_parser
    and+ _ = eof in
    result
end

let result_to_error_message p =
  match p with
  | Ok err -> err
  | Error err -> err

let message_to_layout =
  let open Printf in
  let open Message in
  let open Layout in
  let open Layout.Smart in
  let open Datasource in
  function
  | ReferError (_, FromCrunch path, line, refer_message) ->
    [ argv;
      block 2
        [ "Crunched config parse error!";
          refer_message;
          sprintf "crunched filepath: %s" path;
          sprintf "line: %i" line
        ];
      blank;
      deverror_block
    ]
  | ReferError (_, FromAFile path, line, refer_message) ->
    [ grep path line;
      block 2 [ "Config parse error!"; refer_message ]
    ]
  | CrunchedConfigPath ({ line; filename }, _, path) ->
    [ argv;
      block 2
        [ "Crunched config filepath error!";
          sprintf
            "Attempted to read '%s' out of the crunch." path;
          sprintf "source file: %s" filename;
          sprintf "line: %i" line
        ];
      blank;
      deverror_block
    ]
  | FileReadError (_, msg_from_stdlib) ->
    [ block 0 [ msg_from_stdlib ];
      block 2 [ "Filesystem error reading config file!" ]
    ]
  | AlreadyExists (_, cwd, dir_or_file, path) ->
    let open Filesystem_error in
    let dof =
      match dir_or_file with
      | Dir -> "Directory"
      | File -> "File"
    in
    [ argv;
      block 2
        [ sprintf "%s '%s' already exists in %s." dof path
            cwd
        ]
    ]
  | BadProjectName (_, pname) ->
    [ argv;
      block 2
        [ sprintf "Bad project name: %s" pname;
          "Name must be non-empty and composed only of the \
           following characters:";
          "'A'..'Z', 'a'..'z', '_' or '0'..'9'."
        ]
    ]
  | MissingPrereqs (_, binaries) ->
    [ block 0
        [ "spinup requires the following utilities to be \
           installed:"
        ];
      block 2 binaries
    ]
  | TintSyntaxRecord ({ line; filename }, tint_message) ->
    [ argv;
      block 2
        [ "Error constructing TINT syntax string!";
          tint_message;
          sprintf "source file: %s" filename;
          sprintf "line: %i" line
        ];
      blank;
      deverror_block
    ]
  | TintSyntaxError
      ( _,
        _,
        { Template_error.path; tint_info = func, msg, args }
      ) ->
    let tint_exp =
      "#[" ^ func ^ "," ^ String.concat "," args ^ "]"
    in
    [ argv;
      block 2
        [ "TINT error!";
          sprintf "path: %s" path;
          sprintf "expression: %s" tint_exp;
          msg
        ];
      blank;
      deverror_block
    ]
  | TemplateCrunch path ->
    [ argv;
      block 2
        [ "Template crunch filepath error!";
          sprintf
            "Attempted to read '%s' out of the crunch." path
        ];
      blank;
      deverror_block
    ]
  | ParseError (msg, _, _) ->
    [ block 0 [ sprintf "parse error: %s" msg ] ]

let global_err_to_layout err =
  let open Parser in
  let open Parsers in
  err
  |> eval global_error_parser
  |> result_to_error_message
  |> message_to_layout

let print err =
  err
  |> global_err_to_layout
  |> Layout.to_string
  |> print_endline

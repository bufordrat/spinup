module Message = struct
  type application_layer = Config | Template | Filesystem

  type t =
    | ReferError of
        application_layer
        * Action_error.DataSource.t
        * int
        * string
    | CrunchedConfigPath of
        Lineinfo.t * application_layer * string
    | BadFilePath of application_layer * string
    | FileReadError of application_layer * string
    | AlreadyExists of
        application_layer
        * string
        * Filesystem_error.dir_or_file
        * string
    | TintSyntaxRecord of Lineinfo.t * string
    | TintSyntaxError of
        Action_error.DataSource.t
        * application_layer
        * Template_error.tint_syntax
    | TemplateCrunch of string
    | ParseError of
        string * Global_error.error option * Global_error.t
end

module R = Etude.Result.Make (Message)

module Parser = struct
  module P = struct
    type message = Message.t

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

  let run prsr input = prsr input
  let eval prsr input = R.map fst (prsr input)
  let exec prsr input = R.map snd (prsr input)

  let satisfy pred =
    let open Message in
    function
    | tok :: toks ->
      if pred tok
      then Ok (tok, toks)
      else Error (ParseError ("satisfy", Some tok, toks))
    | [] -> Error (ParseError ("end of input", None, []))

  let optional prsr = prsr <|> pure ()
end

module Examples = struct
  let refer_error_crunch =
    let open Action_error.DataSource in
    [ `ConfigErr;
      `ReferError
        ( FromCrunch ".spinuprc",
          1,
          "continuation line at beginning of record: \" \
           %dune_version 3.13\"" )
    ]

  let refer_error_file =
    let open Action_error.DataSource in
    [ `ConfigErr;
      `ReferError
        ( FromAFile "/home/teichman/.spinuprc",
          1,
          "continuation line at beginning of record: \" \
           %dune_version 3.13\"" )
    ]

  let bad_crunch_path =
    let open Lineinfo in
    [ `ConfigErr;
      `BadCrunchPath
        ( ".spinuprcXXX",
          { line = 20; filename = "lib/config.ml" } )
    ]

  let file_read_error =
    [ `ConfigErr;
      `FileReadError
        "/home/teichman/.spinuprc: Permission denied"
    ]

  let already_exists =
    [ `FilesystemErr;
      `AlreadyExists
        ( "/home/teichman/Code/GitHub/spinup/lib",
          Filesystem_error.Dir,
          "keith" )
    ]

  let bad_syntax =
    let open Lineinfo in
    [ `TemplateErr;
      `BadSyntaxRecord
        ( { line = 9; filename = "lib/template.ml" },
          "invalid syntax: 7 chars <> 4" )
    ]

  let tint_syntax =
    [ `TemplateErr;
      `TintSyntaxError
        { Template_error.path = "/dune-project";
          tint_info =
            ( "nathan",
              "no such function",
              [ "cl"; "dune_version" ] )
        }
    ]
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

  let parse =
    let open Parser in
    refer_error_parser
    <|> tint_syntax_error_parser
    <|> already_exists_parser
end

let parse = Parsers.parse

let result_to_error_message p =
  let open R in
  let* parsed, _ = p in
  Error parsed

let message_to_layout =
  let open Printf in
  let open Message in
  (* let open Lineinfo in *)
  let open Layout in
  let open Layout.Smart in
  let open Action_error.DataSource in
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
  | _ -> assert false

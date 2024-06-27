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
    | TintSyntaxString of Lineinfo.t * string
    | TintSyntaxError of
        Action_error.DataSource.t
        * application_layer
        * Template_error.tint_syntax
    | TemplateCrunch of string
    | ErrorMessageParse of
        string * Global_error.error option * Global_error.t

  type error =
    | ParseError of
        string * Global_error.error option * Global_error.t
end

module MessageError = struct
  type t = Message.error
end

module R = Etude.Result.Make (MessageError)

module Parser = struct
  module P = struct
    type message = Message.t

    type 'a t =
      Global_error.t ->
      ('a * Global_error.t, Message.error) result
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
      else
        Error (ErrorMessageParse ("satisfy", Some tok, toks))
    | [] ->
      Error (ErrorMessageParse ("end of input", None, []))

  let optional prsr = prsr <|> pure ()
end

let example1 =
  [ `TemplateErr;
    `TintSyntax
      { Template_error.path = "/dune-project";
        tint_info =
          ( "nathan",
            "no such function",
            [ "cl"; "dune_version" ] )
      }
  ]

let example2 =
  [ `FilesystemErr;
    `AlreadyExists
      ( "/home/teichman/Code/GitHub/spinup/lib",
        Filesystem_error.Dir,
        "keith" )
  ]

(* module Parsers = struct *)
(*   [@@@warning "-8"] *)

(*   let config_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ _ = satisfy is_config_err in *)
(*     Config *)

(*   let template_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ _ = satisfy is_template_err in *)
(*     Template *)

(*   let filesystem_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ _ = satisfy is_filesystem_err in *)
(*     Filesystem *)

(*   let application_layer_parser = *)
(*     let open Parser in *)
(* config_parser <|> template_parser <|> *)
(*    filesystem_parser *)

(*   let refer_error_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ layer = application_layer_parser *)
(*     and+ (`ReferError (datasource, line, msg)) = *)
(*       satisfy is_refer_error *)
(*     in *)
(*     ReferError (layer, datasource, line, msg) *)

(*   let tint_syntax_error_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ layer = application_layer_parser *)
(*     and+ (`TintSyntax ({ path; _ } as ts)) = *)
(*       satisfy is_tint_syntax *)
(*     in *)
(*     TintSyntaxError (FromCrunch path, layer, ts) *)

(*   let already_exists_parser = *)
(*     let open Parser in *)
(*     let open Message in *)
(*     let open Global_error.Smart in *)
(*     let+ layer = application_layer_parser *)
(*     and+ (`AlreadyExists (path, dof, pname)) = *)
(*       satisfy is_already_exists *)
(*     in *)
(*     AlreadyExists (layer, path, dof, pname) *)

(*   let parse = *)
(*     let open Parser in *)
(*     refer_error_parser *)
(*     <|> tint_syntax_error_parser *)
(*     <|> already_exists_parser *)
(* end *)

(* let parse = Parsers.parse *)

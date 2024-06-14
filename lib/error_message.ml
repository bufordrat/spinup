(* look up the actual warning *)
(* [@@@ warning "-12"] *)

module Message = struct
  type t =
    | ExampleError
    | ParseError of
        string
        * Global_error.error option
        * Global_error.error list
end

module R = Etude.Result.Make (Message)

module Parser = struct
  module P = struct
    type message = Message.t

    type 'a t =
      Global_error.t ->
      ('a * Global_error.t, message) result
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

let is_template_err = function
  | `TemplateErr -> true
  | _ -> false

let is_tint_syntax = function
  | `TintSyntax _ -> true
  | _ -> false

let is_filesystem_err = function
  | `FilesystemErr -> true
  | _ -> false

let is_already_exists = function
  | `AlreadyExists _ -> true
  | _ -> false

let parser1 =
  let open Parser in
  let+ _ = satisfy is_template_err
  and+ tintything = satisfy is_tint_syntax in
  match tintything with
  | `TintSyntax { path; _ } -> path
  | _ -> assert false

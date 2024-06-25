type error = Global_error_intf.error
type t = Global_error_intf.t

module Smart = struct
  include Action_error.Smart
  include Config_error.Smart
  include Filesystem_error.Smart
  include Template_error.Smart
end

module BottomLevel = struct
  type error = Global_error_intf.BottomLevel.t

  let is = function
    | #error -> true
    | _ -> false

  let deverror_block =
    let open Printf in
    let open Layout.Smart in
    block 0
      [ "This should not have happened.";
        "Please let the project maintainer know about the \
         problem so that they can fix it.";
        sprintf
          "You can send them a copy of this error message \
           at %s."
          Contact.email
      ]

  let error_to_layout =
    let open Printf in
    let open Lineinfo in
    let open Layout.Smart in
    function
    | `ReferCrunch (line, refer_message, path) ->
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
    | `ReferFile (line, refer_message, path) ->
      [ grep path line;
        block 2 [ "Config parse error!"; refer_message ]
      ]
    | `BadCrunchPath (path, { line; filename }) ->
      [ argv;
        block 2
          [ "Crunched config filepath error!";
            sprintf
              "Attempted to read '%s' out of the crunch."
              path;
            sprintf "source file: %s" filename;
            sprintf "line: %i" line
          ];
        blank;
        deverror_block
      ]
    | `FileReadError msg_from_stdlib ->
      [ block 0 [ msg_from_stdlib ];
        block 2 [ "Filesystem error reading config file!" ]
      ]
    | `AlreadyExists (cwd, dir_or_file, path) ->
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
    | `ConstructSyntax ({ line; filename }, tint_message) ->
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
    | `BadSyntaxString ({ line; filename }, tint_message) ->
      [ argv;
        block 2
          [ "Ill-formed TINT syntax object!";
            tint_message;
            sprintf "source file: %s" filename;
            sprintf "line: %i" line
          ];
        blank;
        deverror_block
      ]
    | `TintSyntax
        { Template_error.path; tint_info = func, msg, args }
      ->
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
    | `TemplateCrunch path ->
      (* this case can't be reached, most likely *)
      [ argv;
        block 2
          [ "Template crunch filepath error!";
            sprintf
              "Attempted to read '%s' out of the crunch."
              path
          ];
        blank;
        deverror_block
      ]

  let error_to_string err =
    err |> error_to_layout |> Layout.to_string
end

module TopLevel = struct
  type error = Global_error_intf.TopLevel.t

  let error_to_string = function
    | `TemplateErr -> "Template problem!"
    | `FilesystemErr -> "Filesystem problem!"
    | `ConfigErr -> "Config problem!"
end

let error_to_string = function
  | #BottomLevel.error as b -> BottomLevel.error_to_string b
  | #TopLevel.error as t -> TopLevel.error_to_string t

let to_string errlist =
  let open Prelude in
  let filtered = List.filter BottomLevel.is errlist in
  String.join ~sep:"\n" (map error_to_string filtered)

let print errlist = print_endline (to_string errlist)

let print_res = function
  | Error errlist -> print errlist
  | _ -> ()

module T = struct
  let with_error err x =
    let coerced = (err : [< error] :> error) in
    match x with
    | Ok _ -> x
    | Error errs -> Error (coerced :: errs)

  let new_list err =
    let coerced = (err : [< error] :> error) in
    [ coerced ]

  let new_error err = Error (new_list err)
end

module Specialize (E : sig
  type t
end) =
struct
  module type S = sig
    val with_error :
      E.t ->
      ('a, error list) result ->
      ('a, error list) result

    val new_list : E.t -> error list
    val new_error : E.t -> ('a, error list) result
  end
end

let expose = Fun.id

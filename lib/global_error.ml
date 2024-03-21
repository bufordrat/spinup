module Errlist = Global_error_intf.Errlist

module type TRACE = Global_error_intf.TRACE

type t = Global_error_intf.t

let t_to_string =
  let open Printf in
  function
  | `ReferParsing (i, s) ->
    sprintf "Refer parsing error, line %i:\n%s" i s
  | `ConfigCrunch s ->
    sprintf "Config crunch filepath not found:\n%s" s
  | `FileReadError s -> sprintf "File read error:\n%s" s
  | `DirAlreadyExists s ->
    sprintf "Directory already exists:\n%s" s
  | `SyntaxString s ->
    sprintf "Bad TINT syntax string:\n%s" s
  | `TintSyntax s -> sprintf "TINT syntax error:\n%s" s
  | `TemplateCrunch s ->
    sprintf "Template crunch filepath not found:\n%s" s
  | `TemplateErr -> "Template error:"
  | `FilesystemErr -> "Filesystem error:"

let to_string errlist =
  let open Prelude in
  String.join ~sep:"\n" (map t_to_string errlist)

module T = struct
  type 'a trace = ('a, Global_error_intf.Errlist.t) result

  let coerce e =
    ( e
      : [< Global_error_intf.global_error]
      :> Global_error_intf.global_error )

  let new_error e = Error [ coerce e ]
  let constructor con x = new_error (con x)

  let with_error e = function
    | Ok _ as o -> o
    | Error lst -> Error (coerce e :: lst)
end

module Specialize (E : sig
  type error
end) =
struct
  type 'a new_error = E.error -> ('a, Errlist.t) result

  type 'a with_error =
    E.error ->
    ('a, Errlist.t) result ->
    ('a, Errlist.t) result
end

let export = Fun.id

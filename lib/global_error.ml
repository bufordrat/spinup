module Errlist = Global_error_intf.Errlist

module type TRACE = Global_error_intf.TRACE

type t = Global_error_intf.t

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

  let t_to_string =
    let open Printf in
    function
    | `ReferParsing (i, s) ->
      sprintf "Refer parsing error, line %i: %s" i s
    | `ConfigCrunch s ->
      sprintf "Config crunch filepath not found: %s" s
    | `FileReadERror s -> sprintf "File read error: %s" s
    | `DirAlreadyExists s ->
      sprintf "Directory already exists: %s" s
    | `SyntaxString s ->
      sprintf "Bad TINT syntax string: %s" s
    | `TintSyntax s -> sprintf "TINT syntax error: %s" s
    | `TemplateCrunch s ->
      sprintf "Template crunch filepath not found: %s" s
    | `TemplateErr -> "Template error"
    | `FileSystemErr -> "Filesystem error"

  let errlist_to_string errlist =
    let open Prelude in
    String.join ~sep:"\n" (map t_to_string errlist)
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

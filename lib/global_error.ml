module Errlist = Global_error_intf.Errlist

module type TRACE = Global_error_intf.TRACE

type t = Global_error_intf.t

(* meta-note for this code *)
(* figure out which branches will require __LINE__ and
   __FILE__, then add that to the relevant PV variants *)

let t_to_string =
  let open Printf in
  function
  | `ReferParsing (i, s) ->
    (* this needs the grep format *)
    sprintf "Refer parsing error, line %i:\n%s" i s
  | `CrunchPath p ->
    (* ey oh this case is in fact being reached *)
    sprintf "Config crunch filepath not found:\n%s" p
  | `FileReadError s ->
    (* put this one in grep format since it involves a
       file *)
    sprintf "File read error:\n%s" s
  | `DirAlreadyExists s ->
    (* grep format *)
    sprintf "Directory already exists:\n%s" s
  | `SyntaxString s ->
    sprintf "Bad TINT syntax string:\n%s" s
  | `TintSyntax (s1, s2, slist) ->
    (* some day, this may have a line number, but not
       today *)
    (* this probably also needs __FILE__ and __LINE__ for
       when crunching is involved *)
    (* this needs the grep format; use line 1 since it
       doesn't have a line number *)
    (* this should also be named something other than
       "syntax" *)
    let open Prelude.String in
    let joined = join ~sep:", " slist in
    sprintf "TINT error:\n%s %s \n%s" s1 s2 joined
  | `TemplateCrunch crunch_path ->
    (* do this: *)
    (* use __FILE__ for the source filename *)
    (* use __LINE__ for the source line number *)
    (* | `TemplateCrunch (crunch_path, source_filename,
       linenumber) -> *)
    (* roll my own false assert w/ line number of bad code
       etc. and call it dry rot *)
    sprintf "Template crunch filepath not found:\n%s"
      crunch_path
  | `TemplateErr -> "Template error"
  | `FilesystemErr -> "Filesystem error"
  | `ConfigErr -> "Config error"

(* note to self: make this happen: *)
(* Error [ `TemplateErr, `Crunch, TintSyntax ("dune_project", "action.ml", 14)) ]
 * Error [ `TemplateErr, `FileSystem, TintSyntax ("dune_project", "action.ml", 14)) ]
 * Error [ `FilesystemErr, `DirAlreadyExists "keith_project" ] *)

(* dune_project:1:expr-name, problem-with-it
     This is a crunch!
 *   Template Error! *)

(* /full/path/to/thing.ml:1:error-message *)

(* grep format: *)
(* thing.ml:1:error-message *)
(* directory-name:1:error-message *)

(* grep format with nonexistent file: *)
(* -:error-message *)

(* normal format: *)
(* argv0: error message formatted however I want *)

let to_string errlist =
  let open Prelude in
  String.join ~sep:":\n" (map t_to_string errlist)

let print' errlist = print_endline (to_string errlist)

module T = struct
  type 'a trace = ('a, Global_error_intf.Errlist.t) result

  let coerce e =
    ( e
      : [< Global_error_intf.global_error]
      :> Global_error_intf.global_error )

  let new_error e = Error [ coerce e ]

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

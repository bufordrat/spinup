type error = Global_error_intf.error
type t = Global_error_intf.t

let exe_path = Prelude.argv0

let grep_example =
  "/Users/teichman/tmp/functional-adventure/src/GameIO.hs:54:1: \
   error:"

module BottomLevel = struct
  (* meta-note for this code *)
  (* figure out which branches will require __LINE__ and
     __FILE__, then add that to the relevant PV variants *)

  (* dune_project:1:expr-name, problem-with-it This is a
     crunch! * Template Error! *)

  (* /full/path/to/thing.ml:1:error-message *)

  (* grep format: *)
  (* thing.ml:1:error-message *)
  (* directory-name:1:error-message *)

  (* grep format with nonexistent file: *)
  (* -:error-message *)

  (* normal format: *)
  (* argv0: error message formatted however I want *)

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

  let error_to_string =
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
      |> Layout.to_string
    | `ReferFile (line, refer_message, path) ->
      [ grep path line;
        block 2 [ "Config parse error!"; refer_message ]
      ]
      |> Layout.to_string
    | `ConfigCrunchPath (path, { line; filename }) ->
      [ argv;
        block 2
          [ "Crunched config filepath error!";
            sprintf
              "Attempted to read '%s' out of the crunch."
              path;
            sprintf "source file: %s" filename;
            sprintf "source line: %i" line
          ];
        blank;
        deverror_block
      ]
      |> Layout.to_string
    | `FileReadError s ->
      (* put this one in grep format since it involves a
         file *)
      sprintf "File read error:\n%s" s
    | `DirAlreadyExists s ->
      (* grep format *)
      sprintf "Directory already exists:\n%s" s
    | `SyntaxString s ->
      (* if my calculations are correct, this error should
         never happen *)
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
end

module TopLevel = struct
  type error = Global_error_intf.TopLevel.t

  let error_to_string = function
    | `TemplateErr -> "Template problem!"
    | `FilesystemErr -> "Filesystem problem!"
    | `ConfigErr -> "Config problem!"
end

(* note to self: make this happen: *)
(* Error [ `TemplateErr, `Crunch, TintSyntax ("dune_project", "action.ml", 14)) ]
 * Error [ `TemplateErr, `FileSystem, TintSyntax ("dune_project", "action.ml", 14)) ]
 * Error [ `FilesystemErr, `DirAlreadyExists "keith_project" ] *)

let error_to_string = function
  | #BottomLevel.error as b -> BottomLevel.error_to_string b
  | #TopLevel.error as t -> TopLevel.error_to_string t

let to_string errlist =
  let open Prelude in
  let filtered = List.filter BottomLevel.is errlist in
  String.join ~sep:"\n" (map error_to_string filtered)

let print errlist = print_endline (to_string errlist)

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

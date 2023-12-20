module TintStuff = struct
  let default_syntax = Tint.Types.Syntax.tracstring

  let context_to_state ?(syntax=default_syntax) context =
    let open Etude.Result.Make (String) in
    let* syntax = Tint.Types.Syntax.of_string syntax in
    Tint.Eval.(init ~syntax prims (Forms.forms context))

  let macro_expand ?syntax ~context tint =
    let open Etude.Result.Make (String) in
    let* state = context_to_state ?syntax context in
    let+ (_, processed_string) =
      map_error
        (Tint.Types.error_message state.syntax)
        (Tint.Eval.eval state tint)
    in processed_string

  let process_template_path path template pname =
    let context = [ "pname", pname] in
    let fullpath = path ^ "/" ^ template in
    macro_expand ~syntax:"#[,]" ~context (Prelude.readfile fullpath)
end

module Path = struct
  (* let path = "../templates" *)

  let path =
    "/home/teichman/.config/spinup/templates"
end

let process_template_path' path template context =
  (* let context = [ "pname", project_name ] in *)
  let fullpath = path ^ "/" ^ template in
  TintStuff.macro_expand ~syntax:"#[,]" ~context (Prelude.readfile fullpath)

let process_template' ~template ~context =
  process_template_path' Path.path template context

let process_template ~template ~pname =
  TintStuff.process_template_path Path.path template pname

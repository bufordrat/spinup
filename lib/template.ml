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



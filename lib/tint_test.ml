let context_to_state context =
  Tint.Eval.(init prims (Forms.forms context))

let macro_expand ~context tint =
  let open Etude.Result.Make (String) in
  let* state = context_to_state context in
  let+ (_, processed_string) =
    map_error
      (Tint.Types.error_message state.syntax)
      (Tint.Eval.eval state tint)
  in processed_string



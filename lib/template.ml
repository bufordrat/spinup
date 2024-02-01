module E = struct
  type t = [
    | `SyntaxString of string
    | `TintSyntax of string
    ]
  module Smart = struct
    let syntax_string s = `SyntaxString s
    let tint_syntax s = `TintSyntax s
  end
end

module R = Etude.Result.Make (String)
module R' = Etude.Result.Make (E)

module Engine = struct
  let default_syntax = Tint.Types.Syntax.tracstring

  let map_error = Stdlib.Result.map_error

  let context_to_state ?(syntax=default_syntax) context =
    let open R in
    let* syntax = Tint.Types.Syntax.of_string syntax in
    Tint.Eval.(init ~syntax prims (Forms.forms context))

  let string_to_syntax s =
    let open E.Smart in
    let open Tint.Types.Syntax in
    map_error syntax_string (of_string s)

  let context_to_state' ?(syntax=default_syntax) context =
    (* note: when TINT updates so that init doesn't return a result,
       this code will need to be modified *)
    let open R' in
    let open E.Smart in
    let* syntax = string_to_syntax syntax in
    let string_version =
      Tint.Eval.(init ~syntax prims (Forms.forms context))
    in
    map_error syntax_string string_version

  let macro_expand ?syntax ~context tint =
    let open R in
    let* state = context_to_state ?syntax context in
    let+ (_, processed_string) =
      map_error
        (Tint.Types.error_message state.syntax)
        (Tint.Eval.eval state tint)
    in processed_string

  (* let macro_expand' ?syntax ~context tint =
   *   let open R' in
   *   let open E.Smart in
   *   let* state = context_to_state' ?syntax context in
   *   let prep_error tint_error =
   *     Tint.Types.error_message state.syntax tint_error
   *     |> tint_error
   *   in
   *   let+ (_, processed_string) =
   *     map_error
   *       prep_error
   *       (Tint.Eval.eval state tint)
   *   in processed_string *)

  let expand_string ~context str =
    macro_expand ~syntax:default_syntax ~context str

  (* let expand_template_path path template context =
   *   let open R in
   *   let fullpath = path ^ "/" ^ template in
   *   let* raw_contents =
   *     let open Prelude in
   *     trap Exn.to_string readfile fullpath
   *   in
   *   let append_path s = s ^ "\n" ^ fullpath in
   *   let res = 
   *     expand_string ~context raw_contents
   *   in
   *   map_error append_path res
   *   
   * let expand_template ~template ~context =
   *   expand_template_path Path.path template context *)

  let expand_crunched ~template ~context =
    let open R in
    let* raw_contents =
      Crunched_templates.read template
      |> Crunch.option_to_result template
    in
    expand_string ~context raw_contents
end
  
module Processed = struct
  type t = { write_path : string ;
             data : string ;
             vmessage : string }

  let write { write_path ; data ; vmessage } =
    let open Prelude in
    print vmessage ;
    writefile ~fn:write_path data
end

module Unprocessed = struct
  type t = { template_filename : string ;
             output_filename : string ;
             template_path : string ;
             output_path : string ;
             context : (string * string) list ;
             umessage : string ;
           }

  let expand_filenames unp =
    let open R in
    let open Engine in
    let context = unp.context in
    let template_filename = unp.template_filename in
    let template_path = unp.template_path in
    let+ output_filename =
      expand_string ~context unp.output_filename
    and+ output_path =
      expand_string ~context unp.output_path
    and+ umessage =
      expand_string ~context unp.umessage
    in
    { template_filename ;

      output_filename ;
      template_path ;
      output_path ;
      context ;
      umessage ; }

  let process unp =
    let open R in
    let context = unp.context in
    let* partial = expand_filenames unp in
    let write_path =
      let open Prelude in
      match partial.output_path with
      | "" ->
         Prelude.File.join "." partial.output_filename
      | other -> String.join
                   ~sep:"/"
                   [ "." ;
                     other ;
                     partial.output_filename ]
    in
    let vmessage = partial.umessage in
    let template_path =
      let open Prelude in
      String.join
        ~sep:"/"
        [ partial.output_path ;
          partial.template_filename ]
    in
    let+ data = Engine.expand_crunched
                  ~template:template_path
                  ~context:context
    in Processed.{ write_path ; data ; vmessage }
end

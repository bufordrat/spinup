module R = Etude.Result.Make (String)

module Path = struct
  let path = ""
end

module Engine = struct
  let default_syntax = Tint.Types.Syntax.tracstring

  let context_to_state ?(syntax=default_syntax) context =
    let open R in
    let* syntax = Tint.Types.Syntax.of_string syntax in
    Tint.Eval.(init ~syntax prims (Forms.forms context))

  let macro_expand ?syntax ~context tint =
    let open R in
    let* state = context_to_state ?syntax context in
    let+ (_, processed_string) =
      map_error
        (Tint.Types.error_message state.syntax)
        (Tint.Eval.eval state tint)
    in processed_string

  let expand_template_path path template context =
    let open R in
    let fullpath = path ^ "/" ^ template in
    let* raw_contents =
      let open Prelude in
      trap Exn.to_string readfile fullpath
    in
    let append_path s = s ^ "\n" ^ fullpath in
    let res = 
      macro_expand ~syntax:"#[,]" ~context raw_contents
    in
    map_error append_path res
    
  let expand_template ~template ~context =
    expand_template_path Path.path template context

  let expand_crunched ~template ~context =
    let open R in
    let option_to_result = function
      | Some contents -> Ok contents
      | None -> Error "filesystem crunching error"
    in
    let* raw_contents =
      Crunched.read template
      |> option_to_result
    in
    macro_expand ~syntax:"#[,]" ~context raw_contents
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

  let process unv =
    let open R in
    let context = unv.context in
    let write_path =
      match unv.output_path with
      | "" -> "./" ^ unv.output_filename
      | other -> "./" ^ other ^ "/" ^ unv.output_filename
    in
    let vmessage = unv.umessage in
    let template_path =
      unv.output_path ^ "/" ^ unv.template_filename
    in
    let+ data = Engine.expand_crunched
                  ~template:template_path
                  ~context:context
    in Processed.{ write_path ; data ; vmessage }
end

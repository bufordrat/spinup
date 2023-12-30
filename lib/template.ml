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

  let process_template_path path template context =
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
    
  let process_template ~template ~context =
    process_template_path Path.path template context

  let process_crunched ~template ~context =
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

module Unprocessed = struct
  type t = { template_filename : string ;
             output_filename : string ;
             template_path : string ;
             output_path : string ;
             context : (string * string) list ;
             umessage : string ;
           }
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

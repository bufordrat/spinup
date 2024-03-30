module E = Template_error
module Ty = Tint.Types
module R'' = Etude.Result.Make (Global_error)

module Engine = struct
  let spinup_syntax = "#[,]"
  let map_error = Stdlib.Result.map_error

  let string_to_syntax'' s =
    let open E.Smart in
    let open Ty.Syntax in
    let new_error str = [ syntax_string str ] in
    map_error new_error (of_string s)

  let context_to_state'' ?(syntax = spinup_syntax) context =
    (* note: when TINT updates so that init doesn't return a
       result, this code will need to be modified *)
    let open R'' in
    let open E.Smart in
    let* syntax = string_to_syntax'' syntax in
    let string_version =
      Tint.Eval.(init ~syntax prims (Forms.forms context))
    in
    let new_error str = [ syntax_string str ] in
    map_error new_error string_version

  let macro_expand'' ?(syntax = spinup_syntax) ~context tint
      =
    let open R'' in
    let open E.Smart in
    let* state = context_to_state'' ~syntax context in
    let new_error msg = [ tint_syntax msg ] in
    let+ _, processed_string =
      map_error new_error (Tint.Eval.eval state tint)
    in
    processed_string

  let expand_string ?(syntax = spinup_syntax) ~context str
      =
    macro_expand'' ~syntax ~context str

  let option_to_result'' path =
    let open E.Smart in
    function
    | Some contents -> Ok contents
    | None -> Error [ template_crunch path ]

  let expand_crunched ~template ~context =
    let open R'' in
    let* raw_contents =
      Crunched_templates.read template
      |> option_to_result'' template
    in
    expand_string ~context raw_contents
end

module Processed = struct
  type t =
    { write_path : string;
      data : string;
      vmessage : string
    }

  let write { write_path; data; vmessage } =
    let open Prelude in
    print vmessage ;
    writefile ~fn:write_path data
end

module Unprocessed = struct
  type t =
    { template_filename : string;
      output_filename : string;
      template_path : string;
      output_path : string;
      context : (string * string) list;
      umessage : string
    }

  let expand_filenames unp =
    let open R'' in
    let open Engine in
    let context = unp.context in
    let template_filename = unp.template_filename in
    let template_path = unp.template_path in
    let+ output_filename =
      expand_string ~context unp.output_filename
    and+ output_path =
      expand_string ~context unp.output_path
    and+ umessage = expand_string ~context unp.umessage in
    { template_filename;
      output_filename;
      template_path;
      output_path;
      context;
      umessage
    }

  let process unp =
    let open R'' in
    let context = unp.context in
    let* partial = expand_filenames unp in
    let write_path =
      let open Prelude in
      match partial.output_path with
      | "" -> Prelude.File.join "." partial.output_filename
      | other ->
        String.join ~sep:"/"
          [ "."; other; partial.output_filename ]
    in
    let vmessage = partial.umessage in
    let template_path =
      let open Prelude in
      String.join ~sep:"/"
        [ partial.output_path; partial.template_filename ]
    in
    let+ data =
      Engine.expand_crunched ~template:template_path
        ~context
    in
    Processed.{ write_path; data; vmessage }
end

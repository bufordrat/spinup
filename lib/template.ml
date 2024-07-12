module E = Template_error
module Ty = Tint.Types
module Trace = Global_error.T
module R = Etude.Result.Make (Global_error)

module Engine = struct
  let li, spinup_syntax =
    let lineinfo = Lineinfo.make (__LINE__ + 1) __FILE__ in
    (lineinfo, "#[,]")

  let map_error = Stdlib.Result.map_error

  let string_to_syntax s =
    let open E.Smart in
    let open Trace in
    let open Prelude in
    let open Ty.Syntax in
    map_error
      (new_list << bad_syntax_record li)
      (of_string s)

  let context_to_state ?(syntax = spinup_syntax) context =
    let open R in
    let open E.Smart in
    let open Trace in
    let open Prelude in
    let lineinfo = Lineinfo.make (__LINE__ + 1) __FILE__ in
    (* this error handling is superfluous---covered by
       string_to_syntax---but imposed by TINT *)
    let* syntax = string_to_syntax syntax in
    let string_version =
      Tint.Eval.(init ~syntax prims (Forms.forms context))
    in
    map_error
      (new_list << bad_syntax_record lineinfo)
      string_version

  let macro_expand ~path ?(syntax = spinup_syntax) ~context
      tint =
    let open R in
    let open E.Smart in
    let open Trace in
    let open Prelude in
    let* state = context_to_state ~syntax context in
    let+ _, processed_string =
      map_error
        (new_list << tint_syntax_error path)
        (Tint.Eval.eval state tint)
    in
    processed_string

  let expand_string ~path ?(syntax = spinup_syntax) ~context
      str =
    macro_expand ~path ~syntax ~context str

  let option_to_result path =
    let open E.Smart in
    function
    | Some contents -> Ok contents
    | None -> Error [ template_crunch path ]

  let expand_crunched ~path ~context =
    let open R in
    let* raw_contents =
      Crunched_templates.read path |> option_to_result path
    in
    expand_string ~path ~context raw_contents
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
    let open R in
    let open Engine in
    let context = unp.context in
    let template_filename = unp.template_filename in
    let template_path = unp.template_path in
    let+ output_filename =
      expand_string ~path:template_path ~context
        unp.output_filename
    and+ output_path =
      expand_string ~path:template_path ~context
        unp.output_path
    and+ umessage =
      expand_string ~path:template_path ~context
        unp.umessage
    in
    { template_filename;
      output_filename;
      template_path;
      output_path;
      context;
      umessage
    }

  let process unp =
    let open R in
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
      Engine.expand_crunched ~path:template_path ~context
    in
    Processed.{ write_path; data; vmessage }
end

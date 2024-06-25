module E = Config_error
module R = Etude.Result.Make (Global_error)
module Trace = Global_error.T
module DataSource = Action_error.DataSource

type t =
  { pname : string;
    context : (string * string) list;
    datasource : DataSource.t
  }

let default_paths =
  [ "~/.config/spinup/spinuprc";
    "~/.spinuprc";
    "/etc/spinuprc"
  ]

let crunch_lineinfo, crunch_path =
  let lineinfo = Lineinfo.make (__LINE__ + 1) __FILE__ in
  (lineinfo, ".spinuprc")

let is_default = function
  | { pname = _; context = _; datasource = FromCrunch _ } ->
    true
  | _ -> false

let mk_config
    ?(datasource = DataSource.FromCrunch crunch_path) pname
    old_context =
  let context = ("pname", pname) :: old_context in
  { pname; context; datasource }

let refer_parse datasource str =
  let module ReferErr = struct
    type t = int * string
  end in
  let open Etude.Result.Make (ReferErr) in
  let str_to_lst str =
    let open Prelude in
    Seq.to_list (Refer.Seq.of_string str)
  in
  let collapse db =
    let open Etude.List in
    let each_pair (_, assocs) = assocs in
    db >>= each_pair
  in
  let refer_parsing (x, y) =
    let open DataSource in
    match datasource with
    | FromCrunch path ->
      [ E.Smart.refer_crunch (x, y) path ]
    | FromAFile path -> [ E.Smart.refer_file (x, y) path ]
  in
  sequence (str_to_lst str)
  >>| collapse
  |> map_error refer_parsing

module Crunch = struct
  let option_to_result path = function
    | Some contents -> Ok contents
    | None ->
      let open E.Smart in
      let err = bad_crunch_path path crunch_lineinfo in
      Trace.new_error err

  let get_raw_config path =
    Crunched_config.read path |> option_to_result path

  let get_alist_config path =
    let open R in
    let parse = refer_parse (FromCrunch crunch_path) in
    let process = get_raw_config >=> parse in
    process path

  let get_config pname path =
    let open R in
    let+ context = get_alist_config path in
    mk_config pname context
end

let get_config pname filesystem_paths =
  let open Etude.Config in
  let open DataSource in
  let open E.Smart in
  match get_config_path filesystem_paths with
  | Some p ->
    let open R in
    let trapper exn =
      let str = Prelude.Exn.to_string exn in
      Trace.new_list (file_read_error str)
    in
    let read fpath =
      Prelude.(trap trapper readfile fpath)
    in
    let process = read >=> refer_parse (FromAFile p) in
    let+ context = process p in
    mk_config ~datasource:(FromAFile p) pname context
  | None -> Crunch.get_config pname crunch_path

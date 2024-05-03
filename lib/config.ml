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

let is_default = function
  | { pname = _; context = _; datasource = FromCrunch } ->
    true
  | _ -> false

let mk_config ?(datasource = DataSource.FromCrunch) pname
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
    [ E.Smart.refer_parsing datasource (x, y) ]
  in
  sequence (str_to_lst str)
  >>| collapse
  |> map_error refer_parsing

module FromCrunch = struct
  let option_to_result lineinfo path = function
    | Some contents -> Ok contents
    | None ->
      let open E.Smart in
      Trace.new_error (crunch_path path lineinfo)

  let get_config pname path =
    let open R in
    let open DataSource in
    let read path =
      (* TODO: move this down to line 88 *)
      let lineinfo = Lineinfo.make __LINE__ __FILE__ in
      Crunched_config.read path
      |> option_to_result lineinfo path
    in
    let process = read >=> refer_parse FromCrunch in
    let+ context = process path in
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
  | None -> FromCrunch.get_config pname ".spinuprc"

let print_crunch path =
  let open Crunched_config in
  match read path with
  | Some conf -> print_endline conf
  | None -> ()

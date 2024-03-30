module E = Config_error
module R = Etude.Result.Make (Global_error)
module Trace = Global_error.T

module Which = struct
  type t = Default | FromAFile of string
end

type t =
  { pname : string;
    context : (string * string) list;
    which : Which.t
  }

let default_paths =
  [ "~/.config/spinup/spinuprc";
    "~/.spinuprc";
    "/etc/spinuprc"
  ]

let is_default = function
  | { pname = _; context = _; which = Default } -> true
  | _ -> false

let mk_config ?(which = Which.Default) pname old_context =
  let context = ("pname", pname) :: old_context in
  { pname; context; which }

let refer_parse'' str =
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
  let refer_parsing tup = [ E.Smart.refer_parsing tup ] in
  sequence (str_to_lst str)
  >>| collapse
  |> map_error refer_parsing

module FromCrunch = struct
  let option_to_result path = function
    | Some contents -> Ok contents
    | None ->
      let open E.Smart in
      Trace.new_error (config_crunch path)

  let get_config pname crunch_path =
    let open R in
    let read crunch_path =
      Crunched_config.read crunch_path
      |> option_to_result crunch_path
    in
    let process = read >=> refer_parse'' in
    let+ context = process crunch_path in
    mk_config pname context
end

let get_config pname filesystem_paths =
  let open Etude.Config in
  let open Which in
  let open E.Smart in
  match get_config_path filesystem_paths with
  | Some p ->
    let open R in
    let new_error str = [ file_read_error str ] in
    let trapper exn =
      Prelude.Exn.to_string exn |> new_error
    in
    let read fpath =
      Prelude.(trap trapper readfile fpath)
    in
    let process = read >=> refer_parse'' in
    let+ context = process p in
    mk_config ~which:(FromAFile p) pname context
  | None -> FromCrunch.get_config pname ".spinuprc"

let print_crunch path =
  let open Crunched_config in
  match read path with
  | Some conf -> print_endline conf
  | None -> ()

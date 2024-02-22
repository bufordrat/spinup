module Error = Config_error
module R = Etude.Result.Make (String)
module R' = Etude.Result.Make (Error)

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

let refer_parse str =
  let module E = struct
    type t = int * string
  end in
  let open Etude.Result.Make (E) in
  let collapse db =
    let open Etude.List in
    let each_pair (_, assocs) = assocs in
    db >>= each_pair
  in
  let lst =
    let open Prelude in
    Seq.to_list (Refer.Seq.of_string str)
  in
  sequence lst >>| collapse

let refer_parse' str =
  let open R' in
  let collapse db =
    let open Etude.List in
    let each_pair (_, assocs) = assocs in
    db >>= each_pair
  in
  let lst =
    let open Prelude in
    Seq.to_list (Refer.Seq.of_string str)
    |> List.map (map_error Error.Smart.refer_parsing)
  in
  sequence lst >>| collapse

(* TODO: make the line number message be in UNIX format for
   line number error messages *)
(* /etc/passwd:15:teichman:x:1158:11158::/home/teichman:/usr/bin/fish *)
let e_to_string (i, msg) =
  Printf.sprintf "Refer parse error!\nline %d:\n%s" i msg

let parse str = R.map_error e_to_string (refer_parse str)

module FromCrunch = struct
  let get_config pname crunch_path =
    let open R in
    let read crunch_path =
      Crunched_config.read crunch_path
      |> Crunch.option_to_result crunch_path
    in
    let process = read >=> parse in
    let+ context = process crunch_path in
    mk_config pname context

  let option_to_result path = function
    | Some contents -> Ok contents
    | None ->
      let open Error.Smart in
      Error (config_crunch path)

  let get_config' pname crunch_path =
    let open R' in
    let read crunch_path =
      Crunched_config.read crunch_path
      |> option_to_result crunch_path
    in
    let process = read >=> refer_parse' in
    let+ context = process crunch_path in
    mk_config pname context
end

let get_config pname filesystem_paths =
  let open Etude.Config in
  let open Which in
  match get_config_path filesystem_paths with
  | Some p ->
    let open R in
    let read fpath =
      Prelude.(trap Exn.to_string readfile fpath)
    in
    let process = read >=> parse in
    let+ context = process p in
    mk_config ~which:(FromAFile p) pname context
  | None -> FromCrunch.get_config pname ".spinuprc"

let print_crunch path =
  let open Crunched_config in
  match read path with
  | Some conf -> print_endline conf
  | None -> ()

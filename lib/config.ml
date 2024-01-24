module R = Etude.Result.Make (String)

type t = { pname : string ;
           context : (string * string) list ; }

let mk_config pname old_context =
  let context =
    ("pname", pname) :: old_context
  in 
  { pname ; context }

let refer_parse str =
  let module E =
    struct type t = int * string end
  in
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

let e_to_string (i, msg) =
  Printf.sprintf
    "Refer parse error!\nline %d:\n%s" i msg

let parse str =
  R.map_error e_to_string (refer_parse str)

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
end

let get_config pname filesystem_path =
  if Sys.file_exists filesystem_path
  then let open R in
       let read fpath =
         Prelude.(trap Exn.to_string readfile fpath)
       in
       let process = read >=> parse in
       let+ context = process filesystem_path in
       mk_config pname context
  else (print_endline filesystem_path ; exit 1)
        (* FromCrunch.get_config pname ".spinuprc" *)


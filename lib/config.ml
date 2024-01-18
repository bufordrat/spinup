type t = { context : (string * string) list }

let default_context pname =
  [ "pname", pname ]

let default pname =
  let context = default_context pname
  in { context }

let moar_parsing str =
  let module E = struct type t = int * string end in
  let open Etude.Result.Make (E) in
  let open Prelude.Refer.Seq in
  let collapse db =
    let open Etude.List in
    let each_pair (_, assocs) = assocs in
    db >>= each_pair
  in
  let lst = Prelude.Seq.to_list (of_string str) in
  sequence lst >>| collapse

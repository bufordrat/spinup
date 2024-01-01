let ignore_list = [ ".dir_locals.el" ]

let dirs flist =
  let open Stdlib.Filename in
  let not_dot dir = dir <> "." in
  List.map dirname flist
  |> List.filter not_dot
  |> Prelude.nub


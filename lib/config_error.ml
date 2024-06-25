type t =
  [ `ReferCrunch of int * string * string
  | `ReferFile of int * string * string
  | `BadCrunchPath of string * Lineinfo.t
  | `FileReadError of string ]

module Smart = struct
  let refer_crunch (i, s) path = `ReferCrunch (i, s, path)

  let is_refer_crunch = function
    | `ReferCrunch _ -> true
    | _ -> false

  let refer_file (i, s) path = `ReferFile (i, s, path)

  let is_refer_file = function
    | `ReferFile _ -> true
    | _ -> false

  let bad_crunch_path path lineinfo =
    `BadCrunchPath (path, lineinfo)

  let is_bad_crunch_path = function
    | `BadCrunchPath _ -> true
    | _ -> false

  let file_read_error msg_from_stdlib =
    `FileReadError msg_from_stdlib

  let is_file_read_error = function
    | `FileReadError _ -> true
    | _ -> false
end

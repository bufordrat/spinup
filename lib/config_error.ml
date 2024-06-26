type t =
  [ `ReferError of Action_error.DataSource.t * int * string
  | `BadCrunchPath of string * Lineinfo.t
  | `FileReadError of string ]

module Smart = struct
  let refer_error d (i, s) = `ReferError (d, i, s)

  let is_refer_error = function
    | `ReferError _ -> true
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

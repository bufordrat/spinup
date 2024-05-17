type block =
  | Block of int * string list
  | Grep of { path : string; line : int; col : int option }

type t = block list

module Smart = struct
  let block i lst = Block (i, lst)
  let grep ?col path line = Grep { path; line; col }
end

let block_to_string = function
  | Block (indent, msgs) ->
    let i = String.make indent ' ' in
    let indentMsg m = i ^ m in
    String.concat "\n" (List.map indentMsg msgs)
  | Grep { path; line; col } ->
    let open String in
    let c =
      match col with
      | Some c -> [ string_of_int c ]
      | None -> []
    in
    concat ":" ([ path; string_of_int line ] @ c)

let to_string blocks =
  String.concat "\n" (List.map block_to_string blocks)

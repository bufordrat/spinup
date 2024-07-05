type block =
  | Argv
  | Argv0
  | Block of int * string list
  | Blank
  | Grep of { path : string; line : int; col : int option }

type t = block list

module Smart = struct
  let argv = Argv
  let argv0 = Argv0
  let block i lst = Block (i, lst)
  let blank = Blank
  let grep ?col path line = Grep { path; line; col }
end

let block_to_string = function
  | Block (indent, msgs) ->
    let i = String.make indent ' ' in
    let indentMsg m = i ^ m in
    String.concat "\n" (List.map indentMsg msgs)
  | Blank -> ""
  | Grep { path; line; col } ->
    let open String in
    let c =
      match col with
      | Some c -> [ string_of_int c ]
      | None -> []
    in
    concat ":" ([ path; string_of_int line ] @ c)
  | Argv ->
    let ending =
      match Prelude.argv with
      | [] -> ""
      | _ -> " " ^ String.concat " " Prelude.argv
    in
    Prelude.argv0 ^ ending ^ ":"
  | Argv0 -> Prelude.argv0

let to_string blocks =
  String.concat "\n" (List.map block_to_string blocks)

let deverror_block =
  let open Printf in
  let open Smart in
  block 0
    [ "This should not have happened.";
      "Please let the project maintainer know about the \
       problem so that they can fix it.";
      sprintf
        "You can send them a copy of this error message at \
         %s."
        Contact.email
    ]

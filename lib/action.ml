type action =
  | Write of Template.Processed.t
  | Run of Command.t

let is_write = function
  | Write _ -> true
  | _ -> false

type t = (action, string) result

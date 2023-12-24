type t = { args : string list ;
           cmessage : string ; }

let debug_string { args ; cmessage } =
  let open Prelude in
  let line1 = "Run: " ^ String.join ~sep:" " args in
  let line2 = "Message: " ^ cmessage in
  String.join ~sep:"\n" [line1 ; line2]

let debug_print t = print_endline (debug_string t)

let run { args ; cmessage } =
  let open Prelude in
  let open Unix.Proc in
  match args with
  | [] -> print cmessage
  | _ -> begin
      print cmessage ;
      runfull
        ~err:Prelude.(ignore << read)
        ~reader:Prelude.(ignore << read)
        args
      |> ignore
    end

type verbosity = Quiet | Loud

type t =
  { args : string list;
    cmessage : string;
    verbosity : verbosity
  }

let make args cmessage verbosity =
  { args; cmessage; verbosity }

let run { args; cmessage; verbosity } =
  let open Prelude.Unix.Proc in
  let print msg =
    match verbosity with
    | Quiet -> ()
    | Loud -> print_endline msg
  in
  match args with
  | [] -> print cmessage
  | _ ->
    print cmessage ;
    runfull
      ~err:Prelude.(ignore << read)
      ~reader:Prelude.(ignore << read)
      args
    |> ignore

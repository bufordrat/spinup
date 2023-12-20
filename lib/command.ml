type t = { args : string list ;
           cmessage : string ; }

let write { args ; cmessage } =
  let open Prelude in
  let open Unix.Proc in
  print cmessage ;
  runfull
    ~err:Prelude.(ignore << read)
    ~reader:Prelude.(ignore << read)
    args
  |> ignore


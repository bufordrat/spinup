module Original = struct
  let eval state line =
    match Tint.Eval.(eval (reset state) line) with
    | Error err          -> Tint.Types.string_of_error err |> prerr_endline; state
    | Ok (state, result) -> print_endline result; state

  let each_inline () =
    match In_channel.input_line stdin with
    | None      -> None
    | Some line -> Some (line, ())

  let main () =
    match Tint.Eval.(init prims (Forms.forms ["os",Sys.os_type])) with
    | Error err -> prerr_endline err; exit 1
    | Ok state  -> Seq.unfold each_inline () |> Seq.fold_left eval state |> ignore
end

let context = [ "pname", "example" ; ]

let initialize context =
  let open Tint.Eval in
  init prims (Forms.forms context)

let reducer state line =
  match Tint.Eval.(eval (reset state) line) with
  | Error err ->
     Tint.Types.string_of_error err , state
  | Ok (state, result) ->
     result , state

let each_inline channel =
  match In_channel.input_line channel with
  | None -> None
  | Some line -> Some (line, channel)
  (* | None      -> None
   * | Some line -> Some (line, ()) *)
               
let translate channel () =
  match Tint.Eval.(init prims (Forms.forms ["os",Sys.os_type])) with
  | Error _ -> Seq.empty
  | Ok _  -> Seq.unfold each_inline channel
 (* () |> Seq.fold_left reducer ("", state) |> ignore *)

module Setup = struct
  let initialize context =
    let open Tint.Eval in
    let open Etude.Result.Make (String) in
    let* syntax = Tint.Types.Syntax.of_string "#[,]" in
    init ~syntax:syntax prims (Forms.forms context)

  let each_inline channel =
    match In_channel.input_line channel with
    | None      -> None
    | Some line -> Some (line, channel)
end

module Context = struct
  let context = [ "pname", "example" ; ]
end

module Channel = struct
  let eval oc state line =
    match Tint.Eval.(eval (reset state) line) with
    | Error err          -> Tint.Types.string_of_error err |> prerr_endline; state
    | Ok (state, result) -> Prelude.(without (flip writeline oc) result) ; state

  let write_channels context ic oc =
    let open Setup in
    match initialize context with
    | Error err -> prerr_endline err; exit 1
    | Ok state  -> Seq.unfold each_inline ic
                   |> Seq.fold_left (eval oc) state
                   |> ignore

  let write_p context filepath oc =
    let open_template c =
      write_channels context c oc
    in Prelude.within open_template filepath
end


(* let reducer state line =
 *   match Tint.Eval.(eval (reset state) line) with
 *   | Error err ->
 *      Tint.Types.string_of_error err , state
 *   | Ok (state, result) ->
 *      result , state
 * 
 * let each_inline channel =
 *   match In_channel.input_line channel with
 *   | None -> None
 *   | Some line -> Some (line, channel)
 *   (\* | None      -> None
 *    * | Some line -> Some (line, ()) *\)
 *                
 * let translate channel () =
 *   match Tint.Eval.(init prims (Forms.forms ["os",Sys.os_type])) with
 *   | Error _ -> Seq.empty
 *   | Ok _  -> Seq.unfold each_inline channel
 *  (\* () |> Seq.fold_left reducer ("", state) |> ignore *\) *)

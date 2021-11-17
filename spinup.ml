open Prelude
(* open Lib *)
  
let prep_arg = function
  | [] -> ""
  | x :: _ -> x
              
let () =
  Feather.process "cat" [ "spinup.opam" ] |> Feather.run

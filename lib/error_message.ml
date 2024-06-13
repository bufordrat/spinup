module R = Etude.Result.Make (String)

module Parser = struct
  module type PARSERTYPE = sig
    type 'a t

    val run :
      Global_error.t ->
      'a t ->
      ('a * Global_error.t, string) result
  end

  module Type = struct
    type 'a t =
      Global_error.t -> ('a * Global_error.t, string) result

    let run parser input = parser input
    let eval parser input = R.map fst (run parser input)
    let exec parser input = R.map snd (run parser input)
  end

  (* module ParserMonad = struct *)
  (*   type 'a t = 'a Type.t *)

  (*   let pure a input = Ok (a, input) *)
  (*   let bind mx k input = match mx input with *)
  (*     | Ok result -> (k result) input *)
  (*     | other -> other (\* this is a placeholder *\) *)
  (* end *)

  (* include Etude.Endofunctors.Monad.Make (ParserMonad) *)
end

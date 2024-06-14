module R = Etude.Result.Make (Global_error)
module Trace = Global_error.T

module Parser = struct
  module ParserMonad = struct
    type 'a t =
      Global_error.t ->
      ('a * Global_error.t, Global_error.t) result

    let pure a input = Ok (a, input)

    let bind mx k input =
      let open R in
      let* result, remainder = mx input in
      (k result) remainder
  end

  include Etude.Endofunctors.Monad.Make (ParserMonad)

  let run prsr input = prsr input
  let eval prsr input = R.map fst (prsr input)
  let exec prsr input = R.map snd (prsr input)

  let satisfy pred =
    let open Trace in
    function
    | tok :: toks ->
      if pred tok
      then Ok (tok, toks)
      else new_error (`ErrorParse "satisfy")
    | [] -> new_error (`ErrorParse "end of input")
end

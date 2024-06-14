module Trace = Global_error.T

module Message = struct
  type t =
    | ExampleError
    | ParseError of
        string * Global_error.error option * Global_error.t
end

module Parser = struct
  module R = Etude.Result.Make (Message)

  module ParserMonad = struct
    type message = Message.t

    type 'a t =
      Global_error.t ->
      ('a * Global_error.t, message) result

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
    let open Message in
    function
    | tok :: toks ->
      if pred tok
      then Ok (tok, toks)
      else Error (ParseError ("satisfy", Some tok, toks))
    | [] -> Error (ParseError ("end of input", None, []))
end

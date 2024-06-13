module R = Etude.Result.Make (String)

module Parser = struct
  module ParserMonad = struct
    type 'a t =
      Global_error.t -> ('a * Global_error.t, string) result

    let pure a input = Ok (a, input)

    let bind mx k input =
      match mx input with
      | Ok (result, remainder) -> (k result) remainder
      | Error _ -> Error "error"
  end

  include Etude.Endofunctors.Monad.Make (ParserMonad)
end

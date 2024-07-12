type error = Global_error_intf.error
type t = Global_error_intf.t

module Smart = struct
  include Action_error.Smart
  include Config_error.Smart
  include Filesystem_error.Smart
  include Template_error.Smart
end

module T = struct
  let with_error err x =
    let coerced = (err : [< error] :> error) in
    match x with
    | Ok _ -> x
    | Error errs -> Error (coerced :: errs)

  let new_list err =
    let coerced = (err : [< error] :> error) in
    [ coerced ]

  let new_error err = Error (new_list err)
end

module Specialize (E : sig
  type t
end) =
struct
  module type S = sig
    val with_error :
      E.t ->
      ('a, error list) result ->
      ('a, error list) result

    val new_list : E.t -> error list
    val new_error : E.t -> ('a, error list) result
  end
end

let expose = Fun.id
